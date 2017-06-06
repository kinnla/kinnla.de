// Tabsize: 4

// Tutorium Rechnerorganisation SS 2003
// Framework zum Aufgabenblatt 4 von Till Zoppke (zoppke@inf.fu-berlin.de)

/*
1. Aufgabe: Doppelt verkettete Listen.
			(ist gegenüber dem offiziellen Übungsblatt leicht abgewandelt)
2. Aufgabe: Quicksort implementieren.
3. Aufgabe: Pipelines (nicht vergessen!)

Bitte fügt eure Lösung in diese Datei ein und sendet mir die veränderte Datei per email.

Es sind folgende Listenfunktionen zu implementieren:
- add_last 		(5 Punkte)
- remove_nr 	(7 Punkte)
- remove_last	(2 Punkte)
- get_previous	(3 Punkte)
- get_next		(3 Punkte)

Zusatzpunkte gibt es für jene Funktionen, wenn ihr wollt:
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
Falsche Übergabeparameter sind abzufangen!
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
				| Vorgänger	1		|
				+-------------------+
				| Nachfolger 1		|
				+-------------------+
				| Wert 2			|
				+-------------------+
				| Vorgänger	2		|
				+-------------------+
				| Nachfolger 2		|
				+-------------------+
				| Wert 3			|
				+-------------------+
				| Vorgänger	3		|
				+-------------------+
				| Nachfolger 3		|
				+-------------------+
				|	...				|
				+-------------------+
				|	...				|
				+-------------------+
				|					|

Ein Listenelement besteht aus 3 Zeilen = 3*4 Bytes. Diese Blöcke müssen
nicht unbedingt lückenlos nacheinander im Speicher liegen. Wenn man in
diesem Beispiel das zweite Element löscht, würde man die 12 Byte mit
Nullen überschreiben, und das dritte Element bliebe an Ort und Stelle.

Wenn ein neues Element eingefügt wird, so muss man zunächst einen freien
Speicherplatz finden. Hierzu geht man Block für Block durch und prüft,
ob ein Zeiger den Wert Null hat. Null ist keine gültige Adresse, deshalb
kann man in diesem Fall sicher sein, dass der entsprechende Block nicht
gebraucht wird. Die maximale Anzahl an Elementen ist 64, und zwar genau 64.

Die Funktionen get_previous und get_next funktionieren etwas anders als
auf dem Aufgabenblatt beschrieben. Und zwar merken wir uns bei jedem
erfolgreichen Lesezugriff (get-Funktionen) auf ein Listenelement die Adresse
dieses Listenelementes im Feld "history". Bei get_next wird einfach der
Nachfolger des zuletzt gelesenen Elementes ausgegeben, bei get_previous
entsprechend der Vorgänger. Wenn history den Wert 0 hat, so wird das erste(!)
bzw. das letzte Element ausgegeben. Auf diese Weise kann man die Liste vorwärts
bzw. rückwärts durchiterieren, wie man das von Java kennt. Wenn es keinen
Nachfolger bzw. Vorgänger gibt, so wird -1 zurückgegeben. Beachtet: Wenn das
zuletzt gelesene Element gelöscht wird, setzt man den history-Zeiger auf Null
zurück (vgl. remove_first).

Schaut euch zum Einstieg die vorformulierten Funktionen an.
Sie sind auch zum Ausschlachten gedacht. Man kann sich viele Dinge abschauen.
die Funktion "remove_last" arbeitet ähnlich wie "remove_first", nur eben umgekehrt.

Weiterhin macht man sich das Leben einfacher, wenn man bereits formulierte
Funktionen wiederverwendet. Die Funktion remove_nr könnte z.B. eine
Fallunterscheidung treffen, ob das zu entfernende Element am Anfang der Liste (A),
in der Mitte (B) oder am Ende steht (C). Im Fall A kann remove_first aufgerufen
werden, wenn man diese Zusatzaufgabe angehen möchte. Im Fall C wäre remove_last
zuständig. Bleibt also noch Fall B für remove_nr.

Der Zettel ist wahrscheinlich der aufwändigste im ganzen Semester. Auf dem
5. Übungsblatt gibt es noch ein kleines Programm und wieder Verständnisfragen in
Hinblick auf die Klausur. Nicht verzweifeln, sondern mit Quicksort anfangen,
dann mit remove_last weitermachen. Angehende Assemblerfreaks mögen sich an
den 4 Zusatzaufgaben austoben. Wie beim Framework für mic1 gibt es Funktionen
zum Testen eurer Implementation. Beachtet auch die Debugfunktionen am Ende der
Datei zur Ausgabe eines Vektors (für Quicksort) und zur Ausgabe der Liste.
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

	// Feld der Länge null
	printf("Teste Feld der Laenge null: ");
	len = 0;
	vec = (int*) malloc (len * sizeof (int));
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der Länge eins
	printf("Teste Feld der Laenge eins: ");
	len = 1;
	vec = (int*) malloc (len * sizeof (int));
	*vec = 1;
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der Länge zwei
	printf("Teste Feld der Laenge zwei: ");
	len = 2;
	vec = (int*) malloc (len * sizeof (int));
	vec[0] = 1;
	vec[1] = 0;
	quicksort (vec, len);
	printf ("%s\n", is_sorted (vec, len) ? "ok" : "error");

	// Feld der Länge viele
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

// prüft, ob ein gegebenes Feld sortiert ist.
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

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// Überprüfe get_next;
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Überprüfe get_previous;
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Überprüfe remove_last;
	printf("remove_last()\n");
	remove_last(data_ptr);

	// Überprüfe remove_nr;
	printf("remove_nr(1)\n");
	remove_nr(data_ptr, 1);

	// Hänge ein Element an
	printf("add_last(5)\n");
	add_last (data_ptr, 5);

	// So kann man sich die Liste bequem ausgeben lassen. Gut zum debuggen
	show_list(data_ptr);

	// Überprüfe erstes Element
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Überprüfe remove_nr
	printf("remove_nr (0)\n");
	remove_nr(data_ptr, 0);

	// Überprüfe remove_nr
	printf("remove_nr (2)\n");
	remove_nr(data_ptr, 2);

	// Überprüfe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// Überprüfe remove_nr
	printf("remove_nr (1)\n");
	remove_nr(data_ptr, 1);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// Hänge ein Element an
	printf("add_last(7)\n");
	add_last (data_ptr, 7);

	// Hänge noch ein Element an
	printf("add_last(12)\n");
	add_last (data_ptr, 12);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==2 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==12 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==12 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// Überprüfe remove_last
	printf("remove_last()\n");
	remove_last(data_ptr);

	// Überprüfe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// Hänge noch ein Element an
	printf("add_last(13)\n");
	add_last (data_ptr, 13);

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==13 ? "ok" : "fehler");

	// Überprüfe remove_nr
	printf("remove_nr (2)\n");
	remove_nr(data_ptr, 2);

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==7 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Hänge noch ein Element an
	printf("add_last(4)\n");
	add_last (data_ptr, 4);

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==4 ? "ok" : "fehler");

	// Überprüfe remove_first
	printf("remove_first()\n");
	remove_first(data_ptr);

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// Hänge noch ein Element an
	printf("add_last(17)\n");
	add_last (data_ptr, 17);

	// Überprüfe get_first
	printf("get_first() =\t");
	i = get_first(data_ptr);
	printf("%d\t%s\n", i, i==4 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// jetzt 50x einfügen
	printf("50x add_last\n");
	for (i=1; i<51; ++i)
		add_last (data_ptr, i);

	// Überprüfe remove_first
	printf("remove_first()\n");
	remove_first(data_ptr);

	// Überprüfe remove_nr
	printf("remove_nr (3)\n");
	remove_nr(data_ptr, 3);

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==3 ? "ok" : "fehler");

	// 10x löschen
	printf("10x remove_nr()\n");
	for (i=11; i<21; ++i)
		remove_nr (data_ptr, i);

	// jetzt 24x einfügen
	printf("24x add_last\n");
	for (i=1; i<25; ++i)
		add_last (data_ptr, i);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// Hänge noch ein Element an (das 65.)
	printf("add_last(17)\n");
	add_last (data_ptr, 17);

	// Überprüfe Länge
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

	// füge Element hinzu, aber falsche stelle
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

	// füge Element hinzu, aber wieder falsche stelle
	printf("add_nr(5, 2)\n");
	add_nr(data_ptr, 5, 2);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==0 ? "ok" : "fehler");

	// füge Element hinzu, aber diesmal richtige stelle
	printf("add_nr(5, 1)\n");
	add_nr(data_ptr, 5, 1);

	// Überprüfe Länge
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

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// add_first
	printf("add_first(17)\n");
	add_first (data_ptr, 17);

	// Überprüfe get_nr
	printf("get_nr(1) =\t");
	i = get_nr(data_ptr, 1);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// Überprüfe get_nr
	printf("get_nr(2) =\t");
	i = get_nr(data_ptr, 2);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// get_last, also 5
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==2 ? "ok" : "fehler");

	// jetzt 62x einfügen
	printf("62x add_nr(3)\n");
	for (i=1; i<63; ++i)
		add_nr (data_ptr, i, 3);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// Überprüfe get_nr
	printf("get_nr(3) =\t");
	i = get_nr(data_ptr, 3);
	printf("%d\t%s\n", i, i==62 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==61 ? "ok" : "fehler");

	// remove_nr
	printf("remove_nr (3)\n");
	remove_nr(data_ptr, 3);

	// Überprüfe get_nr
	printf("get_nr(3) =\t");
	i = get_nr(data_ptr, 3);
	printf("%d\t%s\n", i, i==61 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==5 ? "ok" : "fehler");

	// add_first, also letztes
	printf("add_first(64)\n");
	add_first (data_ptr, 64);

	// add_first, also unmöglich
	printf("add_first(65)\n");
	add_first (data_ptr, 65);

	// Überprüfe get_nr
	printf("get_nr(1) =\t");
	i = get_nr(data_ptr, 1);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// füge Element hinzu, aber zuviele!
	printf("add_nr(5, 0)\n");
	add_nr(data_ptr, 5, 0);

	// Überprüfe Länge
	printf("length() =\t");
	i = length(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// Überprüfe get_next
	printf("get_next() =\t");
	i = get_next(data_ptr);
	printf("%d\t%s\n", i, i==17 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==64 ? "ok" : "fehler");

	// Überprüfe get_previous
	printf("get_previous()=\t");
	i = get_previous(data_ptr);
	printf("%d\t%s\n", i, i==-1 ? "ok" : "fehler");

	// get_last, also 1
	printf("get_last() =\t");
	i = get_last(data_ptr);
	printf("%d\t%s\n", i, i==1 ? "ok" : "fehler");

	// Überprüfe get_next, also geht nicht
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
		// Überschreibe gesamten Speicherbereich mit Nullen:
		// Anzahl Elemente ist null
		// Zeiger auf erstes Element (head) ist null
		// Zeiger auf letztes Element (tail) ist null
		// Zeiger auf zuletzt gelesenes Element (history) ist null
		// Keine Einträge, also alles null

		mov ebx, data_ptr;		// lade Speicherzeiger in ebx
		sub ebx, 4;				// verringere Speicherzeiger um 4
								// (dann kann man die Schleife bequemer organisieren)
		mov ecx, 256;			// initialisiere ecx mit 256
								// ecx ist Offset fürs Schreiben.
label:	mov DWORD PTR [ebx + 4*ecx], 0;	// schreibe null in den Speicher
		loop label;				// bis der Zeiger auf null kommt, also 1024 byte voll sind.
								// und wir fertig sind.
	}
}

/************************** length ********************************/

// Gibt die Länge der Liste zurück
int length(char* data_ptr)
{
	__asm
	{
		mov eax, data_ptr;	// Lade Speicherzeiger in eax
		mov eax, [eax];		// Lade Anzahl in eax. Fertig.
	}
}

/***************************** add_nr *******************************/

// fügt ein Element an die Stelle nr in die Liste ein.
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

// Fügt ein Element an den Anfang der Liste ein
// Falls Anzahl = 64, so tue nichts
void add_first(char* data_ptr, int data)
{
	__asm
	{
		// Zusatzaufgabe: 2 Punkte
	}
}

/******************************* add_last ******************************/

// fügt ein Element an das Ende der Liste an.
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
// Wenn das gelöschte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zurückgesetzt.
void remove_nr(char* data_ptr, int nr)
{
	__asm
	{
		// 7 Punkte
	}
}

/******************************** remove_first *****************************/

// entfernt das erste Element aus der Liste, falls eines vorhanden.
// Wenn das gelöschte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zurückgesetzt.
void remove_first(char* data_ptr)
{
	__asm
	{
		// überprüfe zunächst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je ende;				// Falls 0, dann sind wir fertig

		// Überprüfe, ob Anzahl=1 (Sonderfall)
		cmp dword ptr [edi], 1;	// Vergleiche mit 1
		je create;				// Falls Anzahl=1, weiter bei create.

		// Vergleiche mit history, lösche ggf.
		mov eax, [edi + 4]		// Lade Head-Zeiger
		cmp eax, [edi+12];		// Vergleiche zu löschendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten lösche history

		// Verbiege Zeiger, lösche Element und verringere Anzahl.
biege:	mov ebx, [eax + 8]		// lade Zeiger aufs zweite Element
		mov [edi + 4], ebx;		// Schreibe neuen head
		mov [ebx + 4], ebx;		// Verbiege Vorgängerzeiger des neuen ersten Elementes auf sich selbst.
		mov dword ptr [eax], 0;	// Lösche Element
		mov dword ptr [eax+4], 0;	// Lösche Element
		mov dword ptr [eax+8], 0;	// Lösche Element
		dec dword ptr [edi];	// veringere Anzahl
		jmp ende;				// fertig

		// Sonderfall: Liste enthält nur ein Element.
		// Dann ist die Liste nach dem entfernen leer.
create:	push edi;				// Speicherzeiger pushen
		call create_list;		// rufe create_list
		pop edi;				// runterpoppen
ende:
	}
}

/******************************** remove_last *****************************/

// entfernt das letzte Element aus der Liste, falls eines vorhanden.
// Wenn das gelöschte Element das gleiche ist, auf welches history zeigt,
// dann wird history auf 0 zurückgesetzt.
void remove_last(char* data_ptr)
{
	__asm
	{
		// 2 Punkte
	}
}

/******************************** get_nr *****************************/

// Suche in der Liste das Element nr und gebe das Datum zurück.
// Falls die Liste leer ist, so ist der Rückgabewert -1
// Falls nr<1 oder nr>Anzahl, so ist der Rückgabewert -1
// Die Adresse des zurückgegebenen Elementes wird in history geschrieben
int get_nr(char* data_ptr, int nr)
{
	__asm
	{
		// Zusatzaufgabe: 3 Punkte
		mov eax, -1				// Dummy-return
	}
}

/*********************************** get_first *****************************************/

// gibt das erste Element der Liste zurück.
// Wenn die Liste leer ist, wird -1 zurückgegeben.
// Die Adresse des zurückgegebenen Elementes wird in history geschrieben
int get_first(char* data_ptr)
{
	__asm
	{
		// überprüfe zunächst, ob die Liste leer ist
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

// gibt das letzte Element der Liste zurück.
// Wenn die Liste leer ist, wird -1 zurückgegeben.
// Die Adresse des zurückgegebenen Elementes wird in history geschrieben
int get_last(char* data_ptr)
{
	__asm
	{
		// Zusatzaufgabe: 2 Punkte
		mov eax, -1				// Dummy-return
	}
}

/************************************* get_next **************************************/

// gibt den Wert des Elementes zurück, welches der Nachfolger des Elementes ist, auf das
// History zeigt. Falls kein Nachfolger existiert, wird -1 zurückgegeben. Falls History
// den Wert 0 hat, so wird das erste(!) Element der Liste zurückgegeben. Falls die Liste
// leer ist, wird -1 zurückgegeben.
// Die Adresse des zurückgegebenen Elementes wird in history geschrieben.
int get_next(char* data_ptr)
{
	__asm
	{
		// 3 Punkte
		mov eax, -1				// Dummy-return
	}
}

/************************************* get_previous **************************************/

// gibt den Wert des Elementes zurück, welches der Vorgänger des Elementes ist, auf das
// History zeigt. Falls kein Vorgänger existiert, wird -1 zurückgegeben. Falls History
// den Wert 0 hat, so wird das letzte(!) Element der Liste zurückgegeben. Falls die Liste
// leer ist, wird -1 zurückgegeben.
// Die Adresse des zurückgegebenen Elementes wird in history geschrieben.
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

/*************************** Debug-Funktionen für Aufgaben 1, 2 **************************/

// Ausgabe der Datenstruktur für die doppelt verketteten Liste
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

// Ausgabe eines Vektors (für Quicksort)
void show_vector(int size, int* vec)
{
	for (int i=0; i<size; ++i)
	{
		printf("%d\t", vec[i]);
	}
	printf("\n");
}

