/*
 * Das Rahmenprogramm von �bungsblatt 2, Aufgabe 4.
 * Erweitert um:
 * - hallo
 * - iterative exponentiation
 * - rekursive exponentiation
 * +++ till zoppke
 */

/*
 * include-Block. Anweisungen an den Linker, Header-Dateien einzubinden,
 * in denen bibliotheksfunktionen definiert sind.
 */
#include "stdafx.h"		// automatisch vom Studio generierte Header Datei
#include <stdio.h>		// standard input output (printf, ...)
#include <stdlib.h>		// standard library (malloc, ...)

/*
 * Methodendeclarationen. Hier die Signaturen der Funktionen eintragen.
 */
int power2 (int num, int power);
void hallo (char*);
int hochzahl_iter (int basis, int exponent);
int hochzahl_rek (int basis, int exponent);

/*
 * Variablendeklarationen.
 */
char* str;

/*
 * Main-methode. argc = Anzahl der von Konsole �bergebenen Argumente.
 * argv = die �bergebenen Argumente in einem Array von Strings
 *        (eigentlich ein Array von Character-Pointern)
 */
int _tmain(int argc, _TCHAR* argv[])
{
	// Deklaration von zwei Testvariablen
	int x, y;

	// Memory allocation. Speicherplatz f�r 80 Character reservieren
	str = (char*) malloc (sizeof(char) * 80);

	// Aufruf von power2. Ausgabe der Ergebnisses auf die Konsole.
	// %d (d wie dezimal) ist ein Platzhalter, f�r den in der Ausgabe ein
	// integer-Wert eingesetzt wird. Also hier der R�ckgabewert von power2(3, 5).
	printf("3 times 2 to the power of 5 is %d\n", power2(3, 5));

	// Aufruf von hallo. Als Parameter: einem Pointer auf ein Character.
	// Die Funktion hallo �ndert die Werte im Hauptspeicher an der Stelle,
	// Auf die str zeigt. Deshalb kein R�ckgabewert n�tig.
	hallo(str);

	// Ausgabe des Strings. %s (s wie String) ist ein Platzhalter f�r einen String.
	printf("And this I want to say: %s Welt!\n", str);

	// Testl�ufe mit repr�sentativen Werten
	for (x = -1; x < 4; x++)
	{
		for (y = -1; y < 4; y++)
		{
			printf ("%d ^ %d = %d (iterativ) = %d (rekursiv)\n",
				    x, y, hochzahl_iter(x, y), hochzahl_rek(x, y));
		}
	}

	// while-Schleife, die wartet, bis ein Zeichen gelesen wird.
	// Am Anfang wird die leere Eingabe gelesen, deren Ende mit EOF
	// (= end of file) markiert ist. Mit diesem Trick wird verhindert,
	// dass sich die Konsole gleich wieder schlie�t und man die Ausgabe
	// nicht mitbekommt
	while (getchar()==EOF);

	// Am Ende von Main wird ein integer-Wert zur�ckgegeben.
	// 0 hei�t, das alles in Ordnung ist.
	return 0;
}

/*
 * Assemblerfunktion mit zwei integer-Parametern und einem integer
 * als R�ckgabewert.
 */
int power2(int num, int power)
{
	// direktive an den Compiler, dass der folgenede Block Assemblercode
	// enth�lt
	__asm
	{
		// zun�chst einmal laden wir die Parameter aus dem Speicher in unsere
		// Register. Im inline Assembler k�nnen wir die Bezeichner der
		// �bergabe-Parameter direkt verwenden.
		mov eax, num			// ersten Parameter in eax laden
		mov ecx, power			// zweiten Parameter in ecx laden
		shl eax, cl;			// Linksshift von eax um soviele Stellen,
								// wie in cl steht.
	}
	// Die Funktion endet ohne return-Statement oder sonstiges.
	// Als R�ckgabewert wird der Inhalt von eax genommen.
}

/*
 * Assemblerfunktion, die an die Speicherstelle, auf die str zeigt, "Hallo"
 * schreibt. Strings sind nichts als eine Kette von Zeichen im Ascii-Code,
 * die nacheinander im Speicher stehen, und durch eine null, beendet werden.
 */
void hallo(char* str)
{
	__asm
	{
		mov eax, str			// Parameter in register eax laden.

		mov BYTE PTR [eax], 0x48			// register indirekte Adressierung,
								// schreiben in den Arbeitsspeicher
		mov BYTE PTR [eax + 1], 0x61		// register relative Adressierung (indirekt mit Offset)
		mov BYTE PTR [eax + 2], 0x6C
		mov BYTE PTR [eax + 3], 0x6C
		mov BYTE PTR [eax + 4], 0x6F
		mov BYTE PTR [eax + 5], 0x00		// Am Ende die Null schreiben.
	}
}

/*
 * berechnet die exponentiation iterativ
 */
int hochzahl_iter (int basis, int exponent)
{
	__asm
	{
		// lade Parameter in die Register
		mov ebx, basis;			// basiswert (= Faktor) in ebx
		mov ecx, exponent;		// exponent (= Schleifenz�hler) in ecx

		// Pr�fe, ob Exponent negativ
		mov eax, 0;				// R�ckgabewert 0
		cmp ecx, 0;				// setze flags f�r Exponenten
		js hiter2;				// Falls negativ, springe ans Ende

		mov eax, 1;				// in eax wird das Ergebnis aufmultipliziert
								// Startwert daher 1

hiter1:	dec ecx					// Schleifenz�hler eins runter
		js hiter2				// wenn wir nun negtiv sind, sind wir fertig.
		imul ebx				// Ansonsten einmal aufs Ergebnis drauf multiplizieren.
		jmp hiter1				// Weitere Schleifendurchlauf

hiter2:							// hier ist Ende
	}
}

/*
 * berechnet die exponentiation rekursiv
 */
int hochzahl_rek (int basis, int exponent)
{
	__asm
	{
		// der anfang ist der gleiche wie oben
		// lade Parameter in die Register
		mov eax, basis;			// basiswert (= Faktor) in eax
		mov ebx, exponent;		// exponent (= Schleifenz�hler) in ebx

		// Pr�fe, ob Exponent negativ
		cmp ebx, 0;				// setze flags f�r Exponenten
		js hrec3;				// Falls negativ, springe zu return 0

		// Pr�fe, ob Exponent gleich null
		// dies ist beim rekursiven Aufruf der Boden der Rekursion.
		// dann m�ssen wir 1 zur�ckgeben, und mit der Multiplikation starten.
		je hrec2;				// Falls null, springe zu return 1

		dec ebx;				// exponent eins runter z�hlen

		// bevor die Funktion nun rekursiv aufgerufen wird, m�ssen wir
		// alle wichtigen Registerwerte sichern. Wir legen sie auf den Stack.
		// hier ist aber nichts zu sichern, denn die Basis �bergeben wir
		// ja als Parameter f�r den Rekursionsaufruf. Die k�nnen wir dann
		// sp�ter wieder runterpoppen

		// jetzt noch die beiden Operanden �bergeben, auch auf den Stack
		push ebx;				// zuerst der zweite Operand (exponent)
		push eax;				// dann der erste (die Basis)
		call hochzahl_rek;		// rekursiver Aufruf

		// nach dem Aufruf liegt der R�ckgabewert in eax
		// jetzt noch zwei mal runterpoppen (f�r die beiden Argumente)
		pop ebx;				// Basiswert in ebx
		pop ecx;				// wird weggeschmissen,
								// nur zum Runterz�hlen des Stackpointers

		// hier w�rde man eventuell gesicherte Register wieder vom Stack nehmen
		// die richtige Reihenfolge der push und pop muss man sich immer wieder
		// aufs Neue klar machen. Last in, first out!

		imul ebx;				// einmal draufmultiplizieren
		jmp hrec4;				// und fertig

hrec2:	mov eax, 1;				// R�ckgabewert 1
		jmp hrec4;				// springe ans Ende

hrec3:	mov eax, 0				// R�ckgabewert 0

hrec4:							// ende
	}
}

/************************** Laufzeitanalyse *********************************

Laufzeit von hochzahl_iter (Anzahl Assemblerbefehle):

- exponent < 0: 6
  (kein Durchlauf der Schleife)

- exponent >= 0: 7 + (exponent + 1) * 4
  (je Schleifendurchlauf 4 Anweisungen, 7 vor bzw. nach der Schleife.)


Laufzeit von hochzahl_rek (Anzahl Assemblerbefehle):

- exponent < 0: 6
  (kein rekursiver Aufruf)

- exponent > 0: 8 + exponent * 14
  (14 Anweisungen je rekursiver Aufruf mit exponent > 0,
   8 Anweisungen f�r Aufruf mit exponent = 0.)

Asymptotisch gesehen, ist die iterative Variante 3,5 mal so schnell wir die rekursive.

*/
