// Tabsize: 4

// Tutorium Rechnerorganisation SS 2003
// Framework zum Aufgabenblatt 4 von Till Zoppke (zoppke@inf.fu-berlin.de)

/*
1. Aufgabe: Doppelt verkettete Listen.
			(ist gegen�ber dem offiziellen �bungsblatt leicht abgewandelt)
2. Aufgabe: Quicksort implementieren.
3. Aufgabe: Pipelines (nicht vergessen!)

Bitte f�gt eure L�sung in diese Datei ein und sendet mir die ver�nderte Datei per email.

Es sind folgende Listenfunktionen zu implementieren:
- add_last 		(5 Punkte)
- remove_nr 	(7 Punkte)
- remove_last	(2 Punkte)
- get_previous	(3 Punkte)
- get_next		(3 Punkte)

Zusatzpunkte gibt es f�r jene Funktionen, wenn ihr wollt:
- add_nr		(3 Punkte)
- add_first		(2 Punkte)
- get_nr		(3 Punkte)
- get_last		(2 Punkte)

Gegeben sind diese Funktionen:
- create_list
- length
- get_first
- remove_first

Spezifikation der Funktionen an Ort und Stelle.
Falsche �bergabeparameter sind abzufangen!
Die Liste ist wie folgt als Datentyp organisiert (Eine Zeile = 4 bytes):

				+-------------------+
data_ptr ---->	| Anzahl			|
				+-------------------+
				| Head				|
				+-------------------+
				| Tail				|
				+-------------------+
				| History			|
				+-------------------+
				| Wert 1			|
				+-------------------+
				| Vorg�nger	1		|
				+-------------------+
				| Nachfolger 1		|
				+-------------------+
				| Wert 2			|
				+-------------------+
				| Vorg�nger	2		|
				+-------------------+
				| Nachfolger 2		|
				+-------------------+
				| Wert 3			|
				+-------------------+
				| Vorg�nger	3		|
				+-------------------+
				| Nachfolger 3		|
				+-------------------+
				|	...				|
				+-------------------+
				|	...				|
				+-------------------+
				|					|

Ein Listenelement besteht aus 3 Zeilen = 3*4 Bytes. Diese Bl�cke m�ssen
nicht unbedingt l�ckenlos nacheinander im Speicher liegen. Wenn man in
diesem Beispiel das zweite Element l�scht, w�rde man die 12 Byte mit
Nullen �berschreiben, und das dritte Element bliebe an Ort und Stelle.

Wenn ein neues Element eingef�gt wird, so muss man zun�chst einen freien
Speicherplatz finden. Hierzu geht man Block f�r Block durch und pr�ft,
ob ein Zeiger den Wert Null hat. Null ist keine g�ltige Adresse, deshalb
kann man in diesem Fall sicher sein, dass der entsprechende Block nicht
gebraucht wird. Die maximale Anzahl an Elementen ist 64, und zwar genau 64.

Die Funktionen get_previous und get_next funktionieren etwas anders als
auf dem Aufgabenblatt beschrieben. Und zwar merken wir uns bei jedem
erfolgreichen Lesezugriff (get-Funktionen) auf ein Listenelement die Adresse
dieses Listenelementes im Feld "history". Bei get_next wird einfach der
Nachfolger des zuletzt gelesenen Elementes ausgegeben, bei get_previous
entsprechend der Vorg�nger. Wenn history den Wert 0 hat, so wird das erste(!)
bzw. das letzte Element ausgegeben. Auf diese Weise kann man die Liste vorw�rts
bzw. r�ckw�rts durchiterieren, wie man das von Java kennt. Wenn es keinen
Nachfolger bzw. Vorg�nger gibt, so wird -1 zur�ckgegeben. Beachtet: Wenn das
zuletzt gelesene Element gel�scht wird, setzt man den history-Zeiger auf Null
zur�ck (vgl. remove_first).

Schaut euch zum Einstieg die vorformulierten Funktionen an.
Sie sind auch zum Ausschlachten gedacht. Man kann sich viele Dinge abschauen.
die Funktion "remove_last" arbeitet �hnlich wie "remove_first", nur eben umgekehrt.

Weiterhin macht man sich das Leben einfacher, wenn man bereits formulierte
Funktionen wiederverwendet. Die Funktion remove_nr k�nnte z.B. eine
Fallunterscheidung treffen, ob das zu entfernende Element am Anfang der Liste (A),
in der Mitte (B) oder am Ende steht (C). Im Fall A kann remove_first aufgerufen
werden, wenn man diese Zusatzaufgabe angehen m�chte. Im Fall C w�re remove_last
zust�ndig. Bleibt also noch Fall B f�r remove_nr.

Der Zettel ist wahrscheinlich der aufw�ndigste im ganzen Semester. Auf dem
5. �bungsblatt gibt es noch ein kleines Programm und wieder Verst�ndnisfragen in
Hinblick auf die Klausur. Nicht verzweifeln, sondern mit Quicksort anfangen,
dann mit remove_last weitermachen. Angehende Assemblerfreaks m�gen sich an
den 4 Zusatzaufgaben austoben. Wie beim Framework f�r mic1 gibt es Funktionen
zum Testen eurer Implementation. Beachtet auch die Debugfunktionen am Ende der
Datei zur Ausgabe eines Vektors (f�r Quicksort) und zur Ausgabe der Liste.
*/

#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>

/************************ method declaration *******************************/

// Signatur von quicksort
void quicksort(int *vec, int len);

// Signatur der Listenfunktionen
void create_list(char* data_ptr);
int length(char* data_ptr);

void add_nr(char* data_ptr, int data, int nr);
void add_first(char* data_ptr, int data);
void add_last(char* data_ptr, int data);

void remove_nr(char* data_ptr, int nr);
void remove_first(char* data_ptr);
void remove_last(char* data_ptr);

int get_nr(char* data_ptr, int nr);
int get_first(char* data_ptr);
int get_last(char* data_ptr);
int get_next(char* data_ptr);
int get_previous(char* data_ptr);

// Signatur der Testfunktionen
void test_quicksort();
bool is_sorted(int* vec, int len);
void test_list();
void test_list_extra();

// Signatur von Debugfunktionen
void show_vector(int size, int* vec);
void show_list(char* ptr);

/****************************** main method **********************************/

// testet Quicksort und die Listenfunktionen
int main(int argc, char* argv[])
{
	// Quicksort testen
	test_quicksort();
	printf("\n");

	// Liste testen
	test_list();
	printf("\n");

	// Zusatzpunkte testen. Bitte ggf einkommentieren
	// test_list_extra();

	// Konsole offenhalten
	while (getchar()==EOF);
	return 0;
}

/******************************* test_quicksort() ***************************/

// testet quicksort mit verschiedenen Aufrufen
void test_quicksort()
{
	int* vec;
	int len;

	printf("teste quicksort...\n");

	// Feld der L�nge null
	printf("Teste Feld der Laenge null: ");
	len = 0;
	vec = (int*) malloc (len * sizeof (int));
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der L�nge eins
	printf("Teste Feld der Laenge eins: ");
	len = 1;
	vec = (int*) malloc (len * sizeof (int));
	*vec = 1;
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der L�nge zwei
	printf("Teste Feld der Laenge zwei: ");
	len = 2;
	vec = (int*) malloc (len * sizeof (int));
	vec[0] = 1;
	vec[1] = 0;
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der L�nge viele
	printf("Teste Feld der Laenge viele: ");
	len = (rand() % 100) + 10;
	vec = (int*) malloc (len * sizeof (int));
	for (int i=0; i<len; ++i)
	{
		vec[i] = rand() % 10;
	}
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");
}

/***************************** is_sorted() ********************************/

// pr�ft, ob ein gegebenes Feld sortiert ist.
bool is_sorted(int* vec, int len)
{
	for (int i=1; i<len; ++i)
	{
		if (vec[i-1] > vec[i])
		{
			return false;
		}
	}
	return true;
}

/****************************** test_list() ************************************/

void test_list()
{
	char* data_ptr = (char*) malloc (1024);
	int i;

	printf("teste liste...\n");

	// Erzeuge Liste
	printf("create_list()\n");
	create_list (data_ptr);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// �berpr�fe get_next;
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// �berpr�fe get_previous;
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// �berpr�fe remove_last;
	printf("remove_last()\n");
	remove_last(data_ptr);

	// �berpr�fe remove_nr;
	printf("remove_nr(1)\n");
	remove_nr(data_ptr, 1);

	// H�nge ein Element an
	printf("add_last(5)\n");
	add_last (data_ptr, 5);

	// So kann man sich die Liste bequem ausgeben lassen. Gut zum debuggen
	show_list(data_ptr);

	// �berpr�fe erstes Element
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// �berpr�fe remove_nr
	printf("remove_nr (0)\n");
	remove_nr(data_ptr, 0);

	// �berpr�fe remove_nr
	printf("remove_nr (2)\n");
	remove_nr(data_ptr, 2);

	// �berpr�fe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// �berpr�fe remove_nr
	printf("remove_nr (1)\n");
	remove_nr(data_ptr, 1);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// H�nge ein Element an
	printf("add_last(7)\n");
	add_last (data_ptr, 7);

	// H�nge noch ein Element an
	printf("add_last(12)\n");
	add_last (data_ptr, 12);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==2 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==12 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==12 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// �berpr�fe remove_last
	printf("remove_last()\n");
	remove_last(data_ptr);

	// �berpr�fe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// H�nge noch ein Element an
	printf("add_last(13)\n");
	add_last (data_ptr, 13);

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==13 ? "ok" : "fehler");

	// �berpr�fe remove_nr
	printf("remove_nr (2)\n");
	remove_nr(data_ptr, 2);

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// H�nge noch ein Element an
	printf("add_last(4)\n");
	add_last (data_ptr, 4);

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==4 ? "ok" : "fehler");

	// �berpr�fe remove_first
	printf("remove_first()\n");
	remove_first(data_ptr);

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// H�nge noch ein Element an
	printf("add_last(17)\n");
	add_last (data_ptr, 17);

	// �berpr�fe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==4 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// jetzt 50x einf�gen
	printf("50x add_last\n");
	for (i=1; i<51; ++i)
		add_last (data_ptr, i);

	// �berpr�fe remove_first
	printf("remove_first()\n");
	remove_first(data_ptr);

	// �berpr�fe remove_nr
	printf("remove_nr (3)\n");
	remove_nr(data_ptr, 3);

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==3 ? "ok" : "fehler");

	// 10x l�schen
	printf("10x remove_nr()\n");
	for (i=11; i<21; ++i)
		remove_nr (data_ptr, i);

	// jetzt 24x einf�gen
	printf("24x add_last\n");
	for (i=1; i<25; ++i)
		add_last (data_ptr, i);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// H�nge noch ein Element an (das 65.)
	printf("add_last(17)\n");
	add_last (data_ptr, 17);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// giving up. any errors detected?
	printf("end of testing list.\n");
}

/****************************** test_list_extra() ******************************/

void test_list_extra()
{
	char* data_ptr = (char*) malloc (1024);
	int i;

	printf("teste Zusatzaufgaben...\n");

	// Erzeuge Liste
	printf("create_list()\n");
	create_list (data_ptr);

	// f�ge Element hinzu, aber falsche stelle
	printf("add_nr(5, 0)\n");
	add_nr(data_ptr, 5, 0);

	// get_first, also nichts
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// get_last, also nichts
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// f�ge Element hinzu, aber wieder falsche stelle
	printf("add_nr(5, 2)\n");
	add_nr(data_ptr, 5, 2);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// f�ge Element hinzu, aber diesmal richtige stelle
	printf("add_nr(5, 1)\n");
	add_nr(data_ptr, 5, 1);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// get_first, also 5
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// get_last, also 5
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// add_first
	printf("add_first(17)\n");
	add_first (data_ptr, 17);

	// �berpr�fe get_nr
	printf("get_nr(1) =\t");
	i = get_nr(data_ptr, 1);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// �berpr�fe get_nr
	printf("get_nr(2) =\t");
	i = get_nr(data_ptr, 2);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// get_last, also 5
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==2 ? "ok" : "fehler");

	// jetzt 62x einf�gen
	printf("62x add_nr(3)\n");
	for (i=1; i<63; ++i)
		add_nr (data_ptr, i, 3);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// �berpr�fe get_nr
	printf("get_nr(3) =\t");
	i = get_nr(data_ptr, 3);
	printf("%d\t%s\n", i, i==62 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==61 ? "ok" : "fehler");

	// remove_nr
	printf("remove_nr (3)\n");
	remove_nr(data_ptr, 3);

	// �berpr�fe get_nr
	printf("get_nr(3) =\t");
	i = get_nr(data_ptr, 3);
	printf("%d\t%s\n", i, i==61 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// add_first, also letztes
	printf("add_first(64)\n");
	add_first (data_ptr, 64);

	// add_first, also unm�glich
	printf("add_first(65)\n");
	add_first (data_ptr, 65);

	// �berpr�fe get_nr
	printf("get_nr(1) =\t");
	i = get_nr(data_ptr, 1);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// f�ge Element hinzu, aber zuviele!
	printf("add_nr(5, 0)\n");
	add_nr(data_ptr, 5, 0);

	// �berpr�fe L�nge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// �berpr�fe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// �berpr�fe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// get_last, also 1
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// �berpr�fe get_next, also geht nicht
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// genug getestet, wird schon stimmen...
	printf("Zusatzaufgaben getestet.\n");

}

/******************************** quicksort ************************************/

void quicksort(int* vec, int len)
{
	__asm
	{
		// 8 Punkte
	}
}

/************************* create_list *******************************/

// erzeugt eine neue leere doppelt verkettete Liste
void create_list(char* data_ptr)
{
	__asm
	{
		// �berschreibe gesamten Speicherbereich mit Nullen:
		// Anzahl Elemente ist null
		// Zeiger auf erstes Element (head) ist null
		// Zeiger auf letztes Element (tail) ist null
		// Zeiger auf zuletzt gelesenes Element (history) ist null
		// Keine Eintr�ge, also alles null

		mov ebx, data_ptr;		// lade Speicherzeiger in ebx
		sub ebx, 4;				// verringere Speicherzeiger um 4
								// (dann kann man die Schleife bequemer organisieren)
		mov ecx, 256;			// initialisiere ecx mit 256
								// ecx ist Offset f�rs Schreiben.
label:	mov DWORD PTR [ebx + 4*ecx], 0;	// schreibe null in den Speicher
		loop label;				// bis der Zeiger auf null kommt, also 1024 byte voll sind.
								// und wir fertig sind.
	}
}

/************************** length ********************************/

// Gibt die L�nge der Liste zur�ck
int length(char* data_ptr)
{
	__asm
	{
		mov eax, data_ptr;	// Lade Speicherzeiger in eax
		mov eax, [eax];		// Lade Anzahl in eax. Fertig.
	}
}

/***************************** add_nr *******************************/

// f�gt ein Element an die Stelle nr in die Liste ein.
// Falls Anzahl = 64, so tue nichts.
// Falls nr < 1, so tue nichts.
// Falls nr > Anzahl+1, so tue nichts
void add_nr(char* data_ptr, int data, int nr)
{
	__asm
	{
		// Zusatzaufgabe: 3 Punkte
	}
}

/******************************* add_first *****************************/

// F�gt ein Element an den Anfang der Liste ein
// Falls Anzahl = 64, so tue nichts
void add_first(char* data_ptr, int data)
{
	__asm
	{
		// Zusatzaufgabe: 2 Punkte
	}
}

/******************************* add_last ******************************/

// f�gt ein Element an das Ende der Liste an.
// Falls schon 64 Elemente vorhanden, ist nichts zu tun.
void add_last(char* data_ptr, int data)
{
	__asm
	{
		// 5 Punkte
	}
}

/**************************** remove_nr **********************************/

// entfernt das mit nr spezifizierte Element
// Falls die Liste leer, wird nichts getan
// Falls nr<0 oder nr>Anzahl, wir nichts getan
// Wenn das gel�schte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zur�ckgesetzt.
void remove_nr(char* data_ptr, int nr)
{
	__asm
	{
		// 7 Punkte
	}
}

/******************************** remove_first *****************************/

// entfernt das erste Element aus der Liste, falls eines vorhanden.
// Wenn das gel�schte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zur�ckgesetzt.
void remove_first(char* data_ptr)
{
	__asm
	{
		// �berpr�fe zun�chst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je ende;				// Falls 0, dann sind wir fertig

		// �berpr�fe, ob Anzahl=1 (Sonderfall)
		cmp dword ptr [edi], 1;	// Vergleiche mit 1
		je create;				// Falls Anzahl=1, weiter bei create.

		// Vergleiche mit history, l�sche ggf.
		mov eax, [edi + 4]		// Lade Head-Zeiger
		cmp eax, [edi+12];		// Vergleiche zu l�schendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten l�sche history

		// Verbiege Zeiger, l�sche Element und verringere Anzahl.
biege:	mov ebx, [eax + 8]		// lade Zeiger aufs zweite Element
		mov [edi + 4], ebx;		// Schreibe neuen head
		mov [ebx + 4], ebx;		// Verbiege Vorg�ngerzeiger des neuen ersten Elementes auf sich selbst.
		mov dword ptr [eax], 0;	// L�sche Element
		mov dword ptr [eax+4], 0;	// L�sche Element
		mov dword ptr [eax+8], 0;	// L�sche Element
		dec dword ptr [edi];	// veringere Anzahl
		jmp ende;				// fertig

		// Sonderfall: Liste enth�lt nur ein Element.
		// Dann ist die Liste nach dem entfernen leer.
create:	push edi;				// Speicherzeiger pushen
		call create_list;		// rufe create_list
		pop edi;				// runterpoppen
ende:
	}
}

/******************************** remove_last *****************************/

// entfernt das letzte Element aus der Liste, falls eines vorhanden.
// Wenn das gel�schte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zur�ckgesetzt.
void remove_last(char* data_ptr)
{
	__asm
	{
		// 2 Punkte
	}
}

/******************************** get_nr *****************************/

// Suche in der Liste das Element nr und gebe das Datum zur�ck.
// Falls die Liste leer ist, so ist der R�ckgabewert -1
// Falls nr<1 oder nr>Anzahl, so ist der R�ckgabewert -1
// Die Adresse des zur�ckgegebenen Elementes wird in history geschrieben
int get_nr(char* data_ptr, int nr)
{
	__asm
	{
		// Zusatzaufgabe: 3 Punkte
		mov eax, -1				// Dummy-return
	}
}

/*********************************** get_first *****************************************/

// gibt das erste Element der Liste zur�ck.
// Wenn die Liste leer ist, wird -1 zur�ckgegeben.
// Die Adresse des zur�ckgegebenen Elementes wird in history geschrieben
int get_first(char* data_ptr)
{
	__asm
	{
		// �berpr�fe zun�chst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (mit -1)

		mov ebx, [edi+4];		// Lade Head in ebx;
		mov [edi+12], ebx;		// Schreibe Head in history
		mov eax, [ebx];			// Lese Datum aus dem Head;
		jmp ende;				// fertig.

minus:	mov eax, -1;
ende:
	}
}

/*********************************** get_last *****************************************/

// gibt das letzte Element der Liste zur�ck.
// Wenn die Liste leer ist, wird -1 zur�ckgegeben.
// Die Adresse des zur�ckgegebenen Elementes wird in history geschrieben
int get_last(char* data_ptr)
{
	__asm
	{
		// Zusatzaufgabe: 2 Punkte
		mov eax, -1				// Dummy-return
	}
}

/************************************* get_next **************************************/

// gibt den Wert des Elementes zur�ck, welches der Nachfolger des Elementes ist, auf das
// History zeigt. Falls kein Nachfolger existiert, wird -1 zur�ckgegeben. Falls History
// den Wert 0 hat, so wird das erste(!) Element der Liste zur�ckgegeben. Falls die Liste
// leer ist, wird -1 zur�ckgegeben.
// Die Adresse des zur�ckgegebenen Elementes wird in history geschrieben.
int get_next(char* data_ptr)
{
	__asm
	{
		// 3 Punkte
		mov eax, -1				// Dummy-return
	}
}

/************************************* get_previous **************************************/

// gibt den Wert des Elementes zur�ck, welches der Vorg�nger des Elementes ist, auf das
// History zeigt. Falls kein Vorg�nger existiert, wird -1 zur�ckgegeben. Falls History
// den Wert 0 hat, so wird das letzte(!) Element der Liste zur�ckgegeben. Falls die Liste
// leer ist, wird -1 zur�ckgegeben.
// Die Adresse des zur�ckgegebenen Elementes wird in history geschrieben.
int get_previous(char* data_ptr)
{
	__asm
	{
		// 3 Punkte
		mov eax, -1				// Dummy-return
	}
}

/************************************ Aufgabe 3 ******************************************/
// Pipelining
// 2 Punkte

/*************************** Debug-Funktionen f�r Aufgaben 1, 2 **************************/

// Ausgabe der Datenstruktur f�r die doppelt verketteten Liste
// Programm von Meike. Auf unsere Datenstruktur angepasst.
void show_list(char* ptr)
{
	int* a =  (int*) ptr;
	int b = (int) a;

	printf("\n\t----------\n");
	printf(" %d: \t %d \t\t <- Anzahl \n", b, a[0]);
	printf(" %d: \t %d \t <- HEAD \n", b+4, a[1]);
	printf(" %d: \t %d \t <- TAIL \n", b+8, a[2]);
	printf(" %d: \t %d \t <- HISTORY \n", b+12, a[3]);

	for(int i=1; i<=4; i++)		// eigentlich bis 64, aber das ist so lang
	{
		printf("\t----------\n");
		// DATA
		printf("%d: \t %d\n", b + 4 + 12*i, a[i*3 + 1]);
        // PREV
		printf("%d: \t %d\n", b + 4 + 12*i+4, a[i*3 + 2]);
        // NEXT
		printf("%d: \t %d\n", b + 4 + 12*i+8, a[i*3 + 3]);
	}
	printf("\n");
}

// Ausgabe eines Vektors (f�r Quicksort)
void show_vector(int size, int* vec)
{
	for (int i=0; i<size; ++i)
	{
		printf("%d\t", vec[i]);
	}
	printf("\n");
}

