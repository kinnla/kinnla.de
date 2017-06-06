\section*{Ausgabe 3}{\bf


zunächst einige nützliche Hilfsfunktionen/Typen: \\
{\bf
%%-----------------------------------------------------------------------------
SFFormat beschreibt wie der Text Formatiert werden soll:
\begin{verbatim}
('l', 15, '~')
  |   |    |
  |   |    +- Zeichen, mit dem bis Länge Aufgefüllt wird
  |   +------ Gewünschte Länge des Textes 
  +---------- Ausrichtung (l-> Links, r-> Rechts, c-> Mitte)
\end{verbatim}}
%%-----------------------------------------------------------------------------
}
\begin{verbatim}

> type SFFormat = (Char, Int, Char)

\end{verbatim}


{\bf
%%-----------------------------------------------------------------------------
stringFormat formatiert einen String wie durch ein SFFormat beschrieben,
             zu lange Strings werden auf die angegebene Länge bschnitten.\\ 
%%-----------------------------------------------------------------------------
}der Algorithmus ist nicht kompliziert aber wie alles was Text Layoutet extrem
häßlich... der einzige Trick ist die lokale Definition t, die sicherstellt das
der eingabetext nie länger als der Ausgabetext wird, und replicate nie negative
parameter bekommt. Für die Ästetiche Wirkung bitte keine Kritik ;-) \\


\begin{verbatim}

> stringFormat :: SFFormat -> String -> String
> stringFormat ('l',size,c) ft = t ++ (replicate (s-(length t)) c) 
>           where t = if ((s - length(ft)) >= 0) then ft else (take s ft)
>                 s = if (size >= 0) then size else (error "cannot format to negative length" )
> stringFormat ('r',size,c) ft = (replicate (s-(length t)) c) ++ t 
>           where t = if ((s - length(ft)) >= 0) then ft else (take s ft)
>                 s = if (size >= 0) then size else (error "cannot format to negative length" )
> stringFormat ('c',size,c) ft = (replicate ((s-(length t)) `div` 2) c) 
>                               ++ (replicate ((s-(length t)) `mod` 2) c) 
>                               ++ t  
>                               ++ (replicate ((s-(length t)) `div` 2) c) 
>           where t = if ((s - length(ft)) >= 0) then ft else (take s ft)
>                 s = if (size >= 0) then size else (error "cannot format to negative length" )
> stringFormat _ _ = error "braindead format given - giving up formating"  

\end{verbatim}



{\bf
%%-----------------------------------------------------------------------------
fp2fs kovertiert einen Float f als String mit einer feste Kommastellenzahl s
%%-----------------------------------------------------------------------------
} Wenn der Float zu wenig Nachkommastellen hat, wird er mit Nullen aufgefüllt,
unnötige Nachkommastellen werden abgeschnitten.

\begin{verbatim}

> fp2fs :: Int -> Float -> String
> fp2fs k f 
>           | k < 0     = error "tried to truncate float where it was sane"   
>           | pcd < k   = ((show f) ++ (replicate (k - pcd) '0'))  
>           | pcd == k  = (show f)
>           | otherwise = take ((length (show f)) - pcd +k) (show f) 
>       where 

              pcd ist die ANzahl der Nachkommastellen

>             pcd = (length (show f) - fcp (show f))

              fcp ist die Position des Dezimalpunktes im String s:se

>             fcp :: String -> Int
>             fcp [] = 1
>             fcp (s:se) 
>                     | s == '.'  = 1
>                     | otherwise = ((fcp se) +1)

\end{verbatim}

Wir brauchen diese nützliche Funktion aber hier nur für 2 Stellen - 
eine prise curry rundet sie dafür ab....

\begin{verbatim}

> fp2s = fp2fs 2

\end{verbatim}


{\bf
%%-----------------------------------------------------------------------------
BillItem: Die Rechnungsposten besteht aus Tripeln (Anzahl, Beschreibung, Preis)\\
Bill: Eine Rechnung aus einer Liste von Rechnungsposten}
      (btw: keine Anspielung Hr. Gates noch die allseits beliebten Kondome...)
%%-----------------------------------------------------------------------------

\begin{verbatim}

> type BillItem = (Int, String, Float)
> type Bill = [BillItem]

\end{verbatim}



{\bf
%%-----------------------------------------------------------------------------
formatBillItem formatiert ein BillItem und gibt es als String zurück
%%-----------------------------------------------------------------------------
}
\begin{verbatim}

> formatBillItem :: BillItem -> String
> formatBillItem (s, d, p) = (stringFormat ('r',5,' ') (show s)) ++ "x "
>                          ++ (stringFormat ('l',30,'.') d) 
>                          ++ (stringFormat ('r',9,'.') ("("++(fp2s p)++")"))
>                          ++ (stringFormat ('r',9,' ') (fp2s (p*(fromInt s))))

\end{verbatim}



{\bf
%%-----------------------------------------------------------------------------
sumBill berechnet die Summe über die Rechnungsposten
%%-----------------------------------------------------------------------------
}
\begin{verbatim}

> sumBill :: Bill -> Float

\end{verbatim}
keine Rechnung $\Rightarrow$ Summe = 0
\begin{verbatim}

> sumBill [] = 0 

\end{verbatim}
ansonsten mit ListComprehension über die Liste iterieren und die Summe 
von allem bilden...
\begin{verbatim}

> sumBill b = sum [ isum i | i <- b]
>         where 
>               isum :: BillItem -> Float
>               isum (s, d, p) = p*(fromInt s)

\end{verbatim}



{\bf
%%-----------------------------------------------------------------------------
printBill gibt die Rechnung bauf dem Bildschrim aus...
%%-----------------------------------------------------------------------------
Als IO-Funktion darf sie quasi imparativ implementiert sein

}
\begin{verbatim}

> printBill :: Bill -> IO ()
> printBill []= error "Money for Nothing is on the DireStraits Album - not here"
> printBill b = do putStr "\nRECHNUNG:\n"
>                  putStr ((replicate 55 '-') ++ "\n") -- linie mit -

\end{verbatim}
wir nutzen ListComprehension um durch die BillItems zu iterieren,
an den String hängen wir ein \\n an und fügen die Liste von Strings mit concat zu einem zusammen 
\begin{verbatim}

>                  putStr (concat [ ((formatBillItem i) ++ "\n") | i <- b])
>                  putStr ((replicate 55 '-') ++ "\n") -- linie mit -


\end{verbatim}
Nun Summe, Mehrwertsteuer, Total, wir nehmen die unten berechnete Summe 
und multiplizieren sie mit den entspr. Faktoren... 
\begin{verbatim}

>                  putStr ((replicate 40 ' ') ++ "SUMME:" 
>                         ++ (stringFormat ('r',9,'.') (fp2s bs))++"\n")
>                  putStr ((replicate 40 ' ') ++ "MWST:" 
>                         ++ (stringFormat ('r',10,'.') (fp2s (bs*0.16)))++"\n")
>                  putStr ((replicate 40 ' ') ++ "TOTAL:" 
>                         ++ (stringFormat ('r',9,'.') (fp2s (bs*1.16)))++"\n")
>            where bs = sumBill b


\end{verbatim}

Testmuster Tippen ist zu anstrengend:

\begin{verbatim}

> testBill :: Bill
> testBill =( ( 23, "Bloody Marry with Coconut Fur and Carpet Split", 13.23)
>            : (  5, "Oliven mit Schlangsahne", 4.07)
>            : (  3, "Massage mit Schlammpackung", 35.73)
>            : (  1, "Pizza Ugly and Oily", 6.10)
>            : (  2, "Schuhe putzen", 2.00)
>            : (  3, "durch den Kakao zehen lassen", 2.00)
>            : ( 13, "Tonicwater", 4.07) : [])

\end{verbatim}


{\bf
%%-----------------------------------------------------------------------------
dieRechnungBitte - we proudly present die Till'sche Funktion:
%%-----------------------------------------------------------------------------
}
\begin{verbatim}

> dieRechnungBitte :: IO()
> dieRechnungBitte = printBill testBill

Testlauf:
        Hugs session for:
        /usr/share/hugs98/lib/Prelude.hs
        alp1-uez-2002-11-28.lhs
        Main> dieRechnungBitte

        RECHNUNG:
        -------------------------------------------------------
           23x Bloody Marry with Coconut Fur ..(13.23)   304.29
            5x Oliven mit Schlangsahne..........(4.07)    20.35
            3x Massage mit Schlammpackung......(35.73)   107.19
            1x Pizza Ugly and Oily..............(6.10)     6.10
            2x Schuhe putzen....................(2.00)     4.00
            3x durch den Kakao zehen lassen.....(2.00)     6.00
           13x Tonicwater.......................(4.07)    52.91
        -------------------------------------------------------
                                                SUMME:...500.84
                                                MWST:.....80.13
                                                TOTAL:...580.97

        Main> 
\end{verbatim}
