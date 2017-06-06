// Tabsize 4

// Tutorium Rechnerorganisation SS 2003
// Musterlösung zum Aufgabenblatt 3 von Till Zoppke (zoppke@inf.fu-berlin.de)

#include "stdafx.h"
#include <stdio.h>

// Signaturen der Funktionen
int gs_iter(int num);
int gs_ohne(int num);
int gs_rec(int num);
int fibo(int n);
int vorth (int *v_a, int *v_b, int len);
void vadd (int *v_a, int *v_b, int *res, int len);

// Variablen für Aufgabe 3
const LEN = 5;
int vec_a [LEN] = {1,2,3,-4,2};
int vec_b [LEN] = {-5, 4, -3, -2, -1};
int ergebnis [LEN] = {0,0,0,0,0};

// Hauptfunktion zum kurzen Testlauf
int _tmain(int argc, _TCHAR* argv[])
{
	// teste Aufgabe 1
	printf("ITERATIV: Gausssummen von 0-7: %d %d %d %d %d %d %d\n", gs_iter(0), gs_iter(1), gs_iter(2),
		   gs_iter(3), gs_iter(4), gs_iter(5), gs_iter(6), gs_iter(7));
	printf("REKURSIV: Gausssummen von 0-7: %d %d %d %d %d %d %d\n", gs_rec(0), gs_rec(1), gs_rec(2),
		   gs_rec(3), gs_rec(4), gs_rec(5), gs_rec(6), gs_rec(7));
	printf("FORMEL: Gausssummen von 0-7: %d %d %d %d %d %d %d\n", gs_ohne(0), gs_ohne(1), gs_ohne(2),
		   gs_ohne(3), gs_ohne(4), gs_ohne(5), gs_ohne(6), gs_ohne(7));
	printf("\n");

	// teste Aufgabe 2
	printf("Fibonacci Zahlen von 0-7: %d %d %d %d %d %d %d\n", fibo(0), fibo(1), fibo(2),
		   fibo(3), fibo(4), fibo(5), fibo(6), fibo(7));
	printf("\n");

	// teste Aufgabe 3
	printf("vorth mit 0: %d \n", vorth(vec_a, vec_b, 0));
	printf("vorth mit LEN: %d \n", vorth(vec_a, vec_b, LEN));
	vadd(vec_a, vec_b, ergebnis, LEN);
	printf("vadd: ");
	for (int i=0; i<LEN; i++)
	printf("%d ", ergebnis[i]);

	// Konsole offenhalten
	while (getchar()==EOF);
	return 0;
}

// iterative Variante der Gaußsumme
int gs_iter(int n)
{
	__asm
	{
		mov ecx, n;			// lade n in Schleifenzähler
		mov eax, 0;			// setze Summe auf 0
		cmp ecx, 0;			// Prüfe Sonderfall: Gaußsumme von 0
		je ende;			// Dann ist 0 das Ergebnis.

label:	add eax, ecx;		// Schleife. Addiere Laufvariable zur Summe hinzu
		loop label;			// Falls wir auf 0 runtergezählt sind, sind wir fertig.
ende :
	}
}

/******************* 1. Aufgabe: Gauß-Summe **********************/

// Gaußsumme rekursiv
int gs_rec(int n)
{
	__asm
	{
		mov eax, n;			// lade n nach eax
		cmp eax, 0;			// Prüfe, ob n = 0
		je ende;			// Dann ist der Rückgabewert 0 (Rekursionsanker)

		dec eax;			// eax = n-1
		push eax;			// Als Parameter auf den Stack
		call gs_rec;		// rekursiver Aufruf

		pop ebx;			// wieder vom Stapel nehmen: n-1 nach ebx
		inc ebx;			// ebx = n
		add eax, ebx;		// eax = gauß (n-1) + n

ende :						// fertig
	}
}

// Gaußsumme nach der Gaußschen Summenformel
int gs_ohne(int n)
{
	__asm
	{
		mov eax, n;			// Lade n in eax
		inc eax;			// eax = n + 1
		imul eax, n;		// eax = n * (n+1)
		shr eax, 1;			// eax = (n*(n+1)) / 2
	}
}

/* Laufzeitberechnung

1. Gauß iterativ:
   - Für n=0: kein Schleifendurchlauf, also 4 Befehle
   - Für n=1: ein Schleifendurchlauf, also 4 + 2 = 6 Befehle
   - Für n>1: n Schleifendurchläufe mit je 2 Befehlen
   - Allgemein für n>=0: Anzahl Befehle bei Eingabe n = 4 + 2*n

2. Gauß rekursiv:
   - Für n=0: kein rekursiver Aufruf, also 3 Befehle
   - Für n=1: ein rekursiver Aufruf, also 9 + 3 = 12 Befehle
   - Für n>1: n rekursive Aufrufe mit je 9 Befehlen
   - Allgemein für n>=0: Anzahl Befehle bei Eingabe n = 3 + 9*n

3. Gauß mit Gaußscher Summenformel:
   - Für jedes n konstant 4 Befehle.
*/

/***************** 2. Aufgabe: Fibonacci-Zahlen *****************/

// Berechnung der n-ten Fibonacci-Zahl
int fibo(int n)
{
	__asm
	{
		mov eax, n;			// lade n in eax
		cmp eax, 1;			// vergleiche mit 1
		jle ende;			// Falls n<=1 so gilt: fibo(n) = n. Rekursionsanker

		dec eax;			// eax = n-1
		push eax;			// Als Parameter auf den Stack
		call fibo;			// rekursiver Aufruf fibo(n-1)

							// Rückgabewert des Aufrufes steht nun in eax
		pop ebx;			// Parameter vom Stack. ebx = n-1
		dec ebx;			// ebx = n-2
		push eax;			// fibo(n-1) auf dem Stack sichern
		push ebx;			// n-2 als Parameter auf den Stack
		call fibo;			// rekursiver Aufruf: fibo(n-2)

							// Rückgabewert des Aufrufes steht nun in eax
		pop ebx;			// n-2 runterpoppen. Wird weggeschmissen.
		pop ebx;			// fibo(n-1) runterpoppen, nach ebx
		add eax, ebx;		// Rückgabewert ist fibo(n) = fibo(n-1) + fibo(n-2)
ende:
	}
}

/******************** 3. Aufgabe: Vektoren ***********************/

// berechnet, ob zwei Vektoren orthogonal zueinander sind.
// Rückgabewert 0, falls Orthogonalität vorliegt. -1 andernfalls.
int vorth (int *v_a, int *v_b, int len)
{
	__asm
	{
		mov edx, v_a;			// lade Adresse von Vektor a
		mov ebx, v_b;			// lade Adresse von Vektor b
		mov ecx, len;			// lade Länge der Vektoren
		mov eax, 0;				// initialisiere Skalarprodukt mit 0

		cmp ecx, 0;				// Vergleiche Länge mit 0
		je ende;				// Falls Länge 0, dann sind die Vektoren orthogonal (Nullvektorraum!)

label:	mov esi, [edx + 4*ecx - 4]	// lade aktuelles Element von Vektor a nach edi
		mov edi, [ebx + 4*ecx - 4]	// lade aktuelles Element von Vektor b nach esi
		imul esi, edi;				// berechne das Produkt
		add eax, esi;				// addiere das Produkt zur Summe
		loop label;					// weiterer Durchlauf mit ecx:=ecx-1 oder Schleife zuende

		cmp eax, 0;				// Vergleiche Skalarprodukt mit 0
		je ende;				// Falls das Sakalarprodukt == 0, dann Rückgabewert 0
		mov eax, -1;			// Andernfalls sind die Vektoren nicht orthogonal, und der Rückgabewert ist -1
ende:
	}
}

// berechnet die Summe zweier Vektoren
void vadd (int *v_a, int *v_b, int *res, int len)
{
	__asm
	{
		mov edx, v_a;			// lade Adresse von Vektor a
		mov ebx, v_b;			// lade Adresse von Vektor b
		mov ecx, len;			// lade Länge der Vektoren
		mov eax, res;			// lade Adresse vom Ergebnisvektor

		cmp ecx, 0;				// Vergleiche Länge mit 0
		je ende;				// Falls Länge 0, dann braucht nichts addiert werden.

label:	mov esi, [edx + 4*ecx - 4]	// Lade aktuellen Eintrag von Vektor a nach esi
		add esi, [ebx + 4*ecx - 4]	// Addiere aktuellen Eintrag von Vektor b hinzu
		mov [eax + 4*ecx - 4], esi	// Schreibe Summe in aktuellen Wintrag des Ergebnisvektors
		loop label;					// Schon fertig oder weiterer Durchlauf mit ecx:=exc-1
ende:
	}
}
