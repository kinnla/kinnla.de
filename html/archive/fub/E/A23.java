package a23;

import java.text.DecimalFormat;
import java.util.Arrays;

// Berechnet n ganze Zahlen, deren Zweiersummen paarweise verschieden sind.
// Die Summen werden danach optimiert, dass die Differenz zwischen der kleinsten
// und der größten Summe möglichst klein ist.
// Lösung von Till Zoppke
public class A23 {

	static final int N = 10;
	static final DecimalFormat prozentFormat = new DecimalFormat("###.0");
	static final DecimalFormat sekundenFormat = new DecimalFormat("##0.000");

	Loesung[] loesungen; // Array mit loesungen der Größe n
	int[] summenLaenge; // Summen-Intervalllaenge der Größe n
	int[] tMaxArray; // Array für kleinstes tMax aller loesungen
	int optimal; // Alle Lösungen bis einschließlich dieser sind optimal
	int nMax; // Maximales n, für das wir eine Lösung suchen
	int[] formel; // Werte der Formel n * (n-1) / 2

	// initialisiert die Datenstruktur
	void init() {
		// loesungen. Loesungen sind zu Beginn null.
		loesungen = new Loesung[nMax + 1];

		// Summendifferenzen. Initialisiere mit Maximalwert.
		summenLaenge = new int[nMax + 1];
		Arrays.fill(summenLaenge, Integer.MAX_VALUE);

		// Am Anfang ist keine Lösung optimal
		optimal = 1;

		// Werte der Formel n * (n-1) / 2
		formel = new int[nMax + 1];
		for (int n = 0; n < formel.length; ++n) {
			formel[n] = n * (n - 1) / 2;
		}

		// kleinste Maximalwerte von t aller Loesungen.
		// Am Anfang 0.
		tMaxArray = new int[nMax + 1];
	}

	// startet den Suchlauf
	void start() {
		int tMax = 2;
		// wiederhole, bis auch für das maximale n eine optimale Lösung gefunden.
		while (optimal < nMax) {
			System.out.println("teste [1.." + tMax + "]");
			// Aufruf der rekursiven Funktion
			versuche(new Loesung(nMax, tMax++));
			// Prüfe, ob eine weitere Lösung optimal geworden.
			if (istOptimal(optimal + 1, tMax)) {
				optimal++;
			}
		}
	}

	// Versucht in eine gegebene Teillösung weitere Zahlen einzufügen.
	void versuche(Loesung loesung) {
		int n = loesung.n;
		if (optimal < n) {
			// Falls für diese Größe noch keine optimale Lösung gefunden
			int laenge = loesung.bestimmeSummenlaenge();
			if (laenge < summenLaenge[n]) {
				// neue beste Loesung für Intervall dieser Größe
				if (loesungen[n] == null) {
					// speichere minimales Intervall, falls erste gefundene Lösung
					tMaxArray[n] = loesung.tMax;
				}
				// speichere Lösung und Summendifferenz
				loesungen[n] = (Loesung) loesung.clone();
				summenLaenge[n] = laenge;
				System.out.println("Neue " + loesungen[n]);
			}
		}
		// Prüfe, ob maximales n bereits erreicht
		if (n < nMax) {
			// kleinste Möglichkeit ist der Nachfolger des zweitgrößten t
			// oder das kleinste Intervall für n Zahlen mit verschiedenen Summen
			int min = Math.max(loesung.vorletztesT() + 1, tMaxArray[n]);

			// größte Möglichkeit ist der Vorgänger des größten t, 
			// abzüglich des Intervalls für die Teillösung der 
			// bis zur nächsten noch nicht optimalen Lösung verbleibenden 
			// Elemente.
			int max = loesung.tMax - 1 - tMaxArray[Math.max(0, optimal - n)];

			// gehe alle Möglichkeiten durch
			for (int t = min; t <= max; ++t) {
				// prüfe, ob zwei Summen gleich sind
				if (!loesung.istKollision(t)) {
					// falls nicht, rekursiver Aufruf mit vergrößerter Teillösung
					loesung.fuegeHinzu(t);
					versuche(loesung);
					loesung.nehmeWeg();
				}
			}
		}
	}

	// prüfe, ob optimaler Wert erreicht.
	boolean istOptimal(int n, int maxT) {
		// Optimaler Wert nach Formel oder minimales Intervall bei maxT
		return summenLaenge[n] == formel[n]
			|| maxT + tMaxArray[n - 2] - 1 >= summenLaenge[n];
	}

	// main-Methode. Kein argument oder argument: n.
	public static void main(String[] args) {
		// Startzeit nehmen
		long startZeit = System.currentTimeMillis();

		// Objekt erschaffen und maximales n bestimmen
		A23 a23 = new A23();
		if (args.length < 1) {
			// falls weniger als 1 argument, dann oben kodiertes n.
			a23.nMax = N;
		} else {
			// wandele erstes Argument in eine Zahl um.
			try {
				a23.nMax = Integer.parseInt(args[0]);
			} catch (NumberFormatException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		// Initialisierung und start
		a23.init();
		a23.start();

		// Ausgabe der Lösungen
		for (int i = 2; i <= a23.nMax; i++) {
			System.out.println(a23.loesungen[i]);
		}

		// Laufzeit messen
		long stoppZeit = System.currentTimeMillis();
		double laufZeit = ((double) (stoppZeit - startZeit)) / 1000;
		System.out.println(
			"Laufzeit: " + sekundenFormat.format(laufZeit) + " Sekunden");
	}

	////////////////////////////// Klasse Loesung ////////////////////////////// 

	// Auch für eine Teillösung 
	class Loesung {
		int[] zahlen; // alle Zahlen dieser Loesung, bis auf die größte
		int tMax; // maximales t, also größte Zahl.
		int n = 2; // Anzahl Zahlen in dieser Loesung
		boolean[] besetzt;
		// Flags, ob die dem Index entsprechende Summe enthalten
		int[] summen; // Zwischenspeicher für die Summen 
		int summenZahl = 1; // Anzahl Summen

		// Konstruktor für die clone()-Methode
		Loesung() {
		}

		// Konstruktor für die Teillösung
		Loesung(int nMax, int tMax) {
			zahlen = new int[nMax - 1];
			zahlen[0] = 1;
			this.tMax = tMax;

			besetzt = new boolean[nMax * nMax * nMax];
			Arrays.fill(besetzt, false);
			besetzt[1 + tMax] = true;

			summen = new int[formel[nMax]];
			summen[0] = 1 + tMax;
		}

		// Differenz der größten zur kleinsten Summe
		private int bestimmeSummenlaenge() {
			if (n == 2) {
				return 1;
			} else {
				return tMax + zahlen[n - 2] - zahlen[1];
			}
		}

		// bestimmt das Verhältnis zwischen theoretischem optimum 
		// und konkreter Summendifferenz
		private double bestimmeGuete() {
			if (n == 2) {
				return 100d;
			} else {
				return 100d
					* ((double) formel[n])
					/ ((double) bestimmeSummenlaenge());
			}
		}

		// entfernt die vorletzte Zahl und alle ihre Summen
		private void nehmeWeg() {
			n--;
			do {
				besetzt[summen[--summenZahl]] = false;
			} while (summenZahl > formel[n]);
		}

		// Fertigt eine kopie des Objektes an.
		// Diese Methode wird gebraucht, um sich Lösungen zu merken.
		public Object clone() {
			Loesung kopie = new Loesung();
			kopie.n = n;
			kopie.summenZahl = summenZahl;
			kopie.tMax = tMax;
			kopie.zahlen = new int[zahlen.length];
			kopie.besetzt = new boolean[besetzt.length];
			System.arraycopy(zahlen, 0, kopie.zahlen, 0, zahlen.length);
			System.arraycopy(besetzt, 0, kopie.besetzt, 0, summenZahl);
			return kopie;
		}

		// Prüft, ob beim Hinzufügen der Zahl t eine Summe doppelt aufträte
		private boolean istKollision(int t) {
			for (int i = 0; i < n - 1; ++i) {
				if (besetzt[zahlen[i] + t]) {
					return true;
				}
			}
			return besetzt[tMax + t];
		}

		// fuegt eine neue Zahl in die Folge ein. Summen werden angepasst.
		private void fuegeHinzu(int neuesT) {
			// fuege neue Summen hinzu
			for (int i = 0; i < n - 1; ++i) {
				besetzt[summen[summenZahl++] = zahlen[i] + neuesT] = true;
			}
			besetzt[summen[summenZahl++] = tMax + neuesT] = true;
			// fuege neue zahl hinzu
			zahlen[n - 1] = neuesT;
			n++;
		}

		// schöne Ausgabe: n, zahlen und summendifferenz
		public String toString() {
			String s = "Loesung fuer n=" + n + ": ";
			s += "[";
			for (int i = 0; i < n - 1; ++i) {
				s = s + zahlen[i] + ",";
			}
			s =
				s
					+ tMax
					+ "]"
					+ "   Intervall: "
					+ bestimmeSummenlaenge()
					+ "   Guete: "
					+ prozentFormat.format(bestimmeGuete())
					+ "%";
			return s;
		}

		// Wert der vorletzten Zahl.
		int vorletztesT() {
			return zahlen[n - 2];
		}
	}
}

/* Testlauf
teste [1..2]
Neue Loesung fuer n=2: [1,2]   Intervall: 1   Guete: 100,0%
teste [1..3]
Neue Loesung fuer n=3: [1,2,3]   Intervall: 3   Guete: 100,0%
teste [1..4]
teste [1..5]
Neue Loesung fuer n=4: [1,2,3,5]   Intervall: 6   Guete: 100,0%
teste [1..6]
teste [1..7]
teste [1..8]
Neue Loesung fuer n=5: [1,2,3,5,8]   Intervall: 11   Guete: 90,9%
teste [1..9]
teste [1..10]
teste [1..11]
teste [1..12]
teste [1..13]
Neue Loesung fuer n=6: [1,2,3,5,8,13]   Intervall: 19   Guete: 78,9%
teste [1..14]
teste [1..15]
teste [1..16]
teste [1..17]
teste [1..18]
teste [1..19]
Neue Loesung fuer n=7: [1,2,3,5,9,14,19]   Intervall: 31   Guete: 67,7%
teste [1..20]
teste [1..21]
teste [1..22]
Neue Loesung fuer n=7: [1,6,8,10,11,14,22]   Intervall: 30   Guete: 70,0%
teste [1..23]
teste [1..24]
teste [1..25]
Neue Loesung fuer n=8: [1,2,3,5,9,15,20,25]   Intervall: 43   Guete: 65,1%
teste [1..26]
teste [1..27]
teste [1..28]
teste [1..29]
teste [1..30]
teste [1..31]
teste [1..32]
teste [1..33]
teste [1..34]
teste [1..35]
Neue Loesung fuer n=9: [1,2,3,5,9,16,25,30,35]   Intervall: 63   Guete: 57,1%
teste [1..36]
teste [1..37]
teste [1..38]
teste [1..39]
Neue Loesung fuer n=9: [1,7,13,21,26,28,29,30,39]   Intervall: 62   Guete: 58,1%
teste [1..40]
teste [1..41]
teste [1..42]
teste [1..43]
teste [1..44]
teste [1..45]
teste [1..46]
Neue Loesung fuer n=10: [1,2,8,11,14,22,27,42,44,46]   Intervall: 88   Guete: 51,1%
teste [1..47]
Neue Loesung fuer n=10: [1,2,3,5,9,16,25,30,35,47]   Intervall: 80   Guete: 56,2%
teste [1..48]
teste [1..49]
teste [1..50]
teste [1..51]
teste [1..52]
teste [1..53]
teste [1..54]
teste [1..55]
Loesung fuer n=2: [1,2]   Intervall: 1   Guete: 100,0%
Loesung fuer n=3: [1,2,3]   Intervall: 3   Guete: 100,0%
Loesung fuer n=4: [1,2,3,5]   Intervall: 6   Guete: 100,0%
Loesung fuer n=5: [1,2,3,5,8]   Intervall: 11   Guete: 90,9%
Loesung fuer n=6: [1,2,3,5,8,13]   Intervall: 19   Guete: 78,9%
Loesung fuer n=7: [1,6,8,10,11,14,22]   Intervall: 30   Guete: 70,0%
Loesung fuer n=8: [1,2,3,5,9,15,20,25]   Intervall: 43   Guete: 65,1%
Loesung fuer n=9: [1,7,13,21,26,28,29,30,39]   Intervall: 62   Guete: 58,1%
Loesung fuer n=10: [1,2,3,5,9,16,25,30,35,47]   Intervall: 80   Guete: 56,2%
Laufzeit: 6,969 Sekunden
*/
