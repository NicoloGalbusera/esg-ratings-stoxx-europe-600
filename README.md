Analisi dei rating ESG nello STOXX Europe 600

Questo repository contiene il codice R utilizzato per le analisi empiriche della mia tesi magistrale:

Struttura e dinamica dei rating ESG in relazione alla performance economico-finanziaria: un’analisi empirica dello STOXX Europe 600.

Lo script analisi_empiriche_esg.R utilizza dati Bloomberg relativi alle società appartenenti allo STOXX Europe 600 e comprende:

- l’analisi dell’evoluzione del rating ESG e delle componenti Environmental, Social e Governance;
- il confronto tra i diversi settori;
- l’analisi della relazione tra rating ESG e indicatori economico-finanziari;
- una replica metodologica adattata mediante Random Forest;
- la previsione dei rating ESG attraverso XGBoost e modelli ARIMA.

Le analisi principali riguardano il periodo 2015-2024.

Per eseguire le analisi è necessario utilizzare R e installare i pacchetti indicati all’interno dello script. I risultati vengono salvati automaticamente nella cartella results.

Il dataset originale non è incluso nel repository perché ottenuto tramite Bloomberg e soggetto alle relative condizioni di licenza.
