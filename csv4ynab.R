#!/usr/bin/Rscript

library(data.table)
library(stringr)

split_csv_quoted <- function(s)
{
  gsub("\"","", strsplit(s,";")[[1]] ,fixed = T)
}

clean_str <- function(s)
{
  str_trim(gsub("[^[:alnum:].-_+*]"," ",str_trim(s)))
}

csvfname=commandArgs(trailing=T)

if(length(csvfname)>0)
{
  csvfname=csvfname[[1]]

# csvfname="hibiscus-export-20151025.csv"
# csvfname="1234________1234.csv"
# csvfname="hibiscus-export-20151209.csv"
# csvfname="1234________1234.csv"

  cat("Reading '",csvfname,"'...",sep="")
  fcon=file(csvfname,"rt", encoding = "CP1252")

  header=readLines(fcon,1)

  ftype=if( grepl("Kreditkarte",header,fixed = T) ) "dkbvisa" else if( grepl("Kontonummer",header,fixed = T) ) "hibiscus" else ""

  result=switch(ftype,
            dkbvisa={
              konto=split_csv_quoted(header)[2]
              finished=FALSE
              while(!finished)
              {
                tmp=readLines(fcon,1,encoding = "CP1252")
                stopifnot(length(tmp)>0)
                if(nchar(tmp)>0)
                {
                  tmp=split_csv_quoted( tmp )
                  switch(tmp[1],
                         `Saldo:`={
                           saldo=gsub(" EUR","",tmp[2],fixed = T)
                         },
                         `Umsatz abgerechnet`={
                           column_names=tmp
                           finished=TRUE
                         }
                  )
                }
              }

              cat("=> DKB VISA\nSaldo:",saldo,"EUR\n")
              dt=data.table(read.table(fcon,
                                      sep=";",
                                      dec=",",
                                      quote="\"",
                                      stringsAsFactors = F,
                                      header = F,
                                      fileEncoding="CP1252"))
              setnames(dt,seq_along(column_names),column_names)
              # Belegdatum Wertstellung
              dt=dt[,.(Datum=as.Date(Belegdatum,"%d.%m.%Y"),
                       Gegenkonto=Beschreibung,
                       Betrag=`Betrag (EUR)`
                       )]
              dt[,Beschreibung:=""]
              list(saldo=saldo,dt=dt)
            },
            hibiscus={
              q = if(substr(header,1,1)=="\"") "\"" else ""
              s = if(length(str_split(header,",")[[1]])>1) "," else ";"
              dt=data.table(read.table(fcon,
                                      sep=s,
                                      dec=",",
                                      quote = q,
                                      stringsAsFactors = F,
                                      header = F))
              cat("=> Hibiscus\n")
              column_names = if(s==";") split_csv_quoted(header) else str_split(header,",")[[1]]
              setnames(dt,seq_along(column_names),column_names)
              stopifnot(dt[,length(unique(Konto))==1],
                        dt[,length(unique(Kontonummer))==1],
                        dt[,length(unique(BLZ))==1])
              konto=dt[1,paste(Konto,Kontonummer,BLZ)]
              saldo=dt[,last(Zwischensumme)]
              dt=dt[,.(Datum=as.Date(Datum,"%d.%m.%Y"),
                       #Gegenkonto=paste(`Gegenkonto Inhaber`,Gegenkonto,`Gegenkonto BLZ`),
                       Gegenkonto=`Gegenkonto Inhaber`,
                       Betrag,
                       Beschreibung=paste(Verwendungszweck,`Verwendungszweck 2`,`Weitere Verwendungszwecke`)
                       # Beschreibung=`Weitere Verwendungszwecke`
                       )]
              list(saldo=saldo,dt=dt)
            }, data.table())

  close(fcon)


  # Date,Payee,Category,Memo,Outflow,Inflow
  # 01/25/12,Sample Payee,,Sample Memo for an outflow,100.00,
  # 01/26/12,Sample Payee 2,,Sample memo for an inflow,,500.00

  dt4ynab=data.table(
    Date=result$dt[,format(Datum,"%d/%m/%y")],
    Payee=result$dt[,clean_str(Gegenkonto)],
    Category="",
    Memo=result$dt[,clean_str(Beschreibung)],
    Outflow=result$dt[,ifelse(Betrag>0,"",formatC(-Betrag,decimal.mark = ".",digits=2,format="f"))],
    Inflow= result$dt[,ifelse(Betrag<0,"",formatC(Betrag,decimal.mark = ".",digits=2,format="f"))]
    )

  ynabfname=paste0("ynab_",csvfname)
  cat("Writing '",ynabfname,"'...\n",sep="")
  write.csv(dt4ynab,file=ynabfname,quote=F,row.names=F)
}
