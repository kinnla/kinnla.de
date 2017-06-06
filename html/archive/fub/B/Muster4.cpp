// Tabsize 4

// Tutorium Rechnerorganisation SS 2003
// Musterlösung zum Aufgabenblatt 4 von Till Zoppke (zoppke@inf.fu-berlin.de)
// Testfunktionen finden sich im Framework

// Signaturen
void quicksort(int *vec, int len);
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


/****************************************************************************/
/**************** Aufgabe 1: Doppelt verkettete Listen **********************/
/****************************************************************************/

/************************* create_list *******************************/

// erzeugt eine neue leere doppelt verkettete Liste
void create_list(char* data_ptr)
{
	__asm
	{
		// Überschreibe gesamten Speicherbereich mit Nullen:
		// Anzahl Elemente ist null
		// Zeiger auf erstes Element ist null
		// Zeiger auf letztes Element ist null
		// Zeiger auf zuletzt gelesenes Element ist null
		// Keine Einträge, also alles null

		mov ebx, data_ptr;		// lade Speicherzeiger in ebx
		sub ebx, 4;				// verringere Speicherzeiger um 4
								// (dann kann man die Schleife bequemer organisieren)
		mov ecx, 256;			// initialisiere ecx mit 256
								// ecx ist Offset fürs Schreiben.
label:	mov DWORD PTR [ebx + 4*ecx], 0;	// schreibe null in den Speicher
		loop label;				// bis der Zeiger auf null kommt
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
		// Lade Variablen in die Register
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		mov edx, data;			// Lade Datum in edx
		mov ecx, nr;			// Lade nr in ecx

		// Prüfe, ob Parameter im gültigen Bereich
		cmp dword ptr [edi],64;	// Vergleiche Anzahl = 64
		je ende;				// Falls = 64, dann kann nichts mehr eingefügt werden, fertig.
		cmp ecx, 1;				// Vergleiche mit 1
		jl ende;				// Falls nr<1, dann fertig
		dec ecx;				// nr = nr-1 (zum Vergleichen)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		jg ende;				// Falls nr>Anzahl, dann fertig
		inc ecx;				// nr = nr+1 (wiederherstellen)

		// Überprüfe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche nr mit 1
		je head;				// Falls nr=1, dann Sonderfall: erstes Element
		dec ecx;				// nr = nr+1 (zum Vergleichen)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl+1, dann Sonderfall: letztes Element
		inc ecx;				// nr = nr-1 (wiederherstellen)

		// suche feien Speicherplatz
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum nächsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden.
		sub esi, 4;				// berechne Zeiger auf neues Element

		// Füge das Element in der Mitte der Liste ein.
		// Laufe zunächst die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx;
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum nächsten Element
		loop laufe;				// Beende Schleife oder suche weiter

		// ebx zeigt jetzt auf das Element, vor dem wir einfügen wollen.
		// verbiege Zeiger, verändere Datum, erhöhe Anzahl
		mov eax, [ebx + 4];		// lade Vorgängerzeiger
		mov [eax + 8], esi;		// verbiege Nachfolgerzeiger des Vorgängers
		mov [ebx + 4], esi;		// verbiege Vorgängerzeiger des Nachfolgers
		mov [esi], edx;			// schreibe Datum
		mov [esi + 4], eax;		// schreibe Vorgängerzeiger
		mov [esi + 8], ebx;		// schreibe Nachfolgerzeiger
		inc dword ptr [edi];	// erhöhe Anzahl
		jmp ende;				// fertig.

		// Element wird an den Anfang eingefügt.
		// Rufe Assemblerfunktion "add_first".
head:	push edx;				// Parameter data
		push edi;				// Parameter data_ptr
		call add_first;			// rekursiver Aufruf
		pop edi;				// runterpoppen
		pop edx;				// runterpoppen
		jmp ende;				// fertig

		// Element wird ans Ende eingefügt.
		// Rufe Assemblerfunktion "add_last".
tail:	push edx;				// Parameter data
		push edi;				// Parameter data_ptr
		call add_last;			// rekursiver Aufruf
		pop edi;				// runterpoppen
		pop edx;				// runterpoppen
ende:
	}
}

/******************************* add_first *****************************/

void add_first(char* data_ptr, int data)
{
	__asm
	{
		// Fallunterscheidung nach Anzahl der Einträge
		mov edi, data_ptr;		// lade Speicherzeiger in edi
		mov edx, data;			// lade Datum in edx
		cmp DWORD PTR [edi],64;	// Prüfe, ob schon 64 Einträge vorhanden
		je ende;				// Falls ja, dann sind wir fertig.
		cmp DWORD PTR [edi],0;	// Prüfe, ob schon ein Eintrag vorhanden
		je neu;					// falls nicht, gehe zu neu.

		// Anzahl der Einträge ist 0 < Anzahl < 64
		// erhöhe Anzahl und suche einen freien Speicherplatz
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum nächsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden. Verbiege Zeiger und schreibe Wert.
		sub esi, 4;				// berechne Zeiger auf neues Element
		mov eax, [edi + 4]		// Lade Head-Zeiger
		mov [eax + 4], esi;		// verbiege Vorgängerzeiger des ehemaligen ersten Elementes
		mov [edi + 4], esi;		// Verbiege head-Zeiger
		mov [esi], edx;			// schreibe Datum
		mov [esi + 4], esi;		// schreibe Vorgängerzeiger (auf sich selbst)
		mov [esi + 8], eax;		// schreibe Nachfolgerzeiger (auf ehemaliges erstes)
		inc dword ptr [edi];	// erhöhe Anzahl
		jmp ende;				// fertig

// Die Liste ist leer. Wir schreiben unser Elemt an die erste Stelle.
neu:	mov dword ptr [edi], 1;		// neue Anzahl ist 1
		mov eax, edi;				// Speicherpointer in eax
		add eax, 16;				// berechne Pointer auf erstes element
		mov dword ptr [edi+4], eax;	// neuer Head ist eax
		mov dword ptr [edi+8], eax;	// neuer Tail ist eax
		mov [eax], edx;				// schreibe Datum
		mov dword ptr [eax+4], eax;	// Vorgängerzeiger ist eax
		mov dword ptr [eax+8], eax;	// Nachfolgerzeiger ist eax
ende:
	}
}

/******************************* add_last ******************************/

// fügt ein Element an das Ende der Liste an.
// Falls schon 64 Elemente vorhanden, ist nichts zu tun.
void add_last(char* data_ptr, int data)
{
	__asm
	{
		// Fallunterscheidung nach Anzahl der Einträge
		mov edi, data_ptr;		// lade Speicherzeiger in edi
		mov edx, data;			// lade Datum in edx
		cmp DWORD PTR [edi],64;	// Prüfe, ob schon 64 Einträge vorhanden
		je ende;				// Falls ja, dann sind wir fertig.
		cmp DWORD PTR [edi],0;	// Prüfe, ob schon ein Eintrag vorhanden
		je neu;					// falls nicht, gehe zu neu.

		// Anzahl der Einträge ist 0 < Anzahl < 64
		// erhöhe Anzahl und suche einen freien Speicherplatz
		inc dword ptr [edi]	// inkrementiere Anzahl
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum nächsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden. Verbiege Zeiger und schreibe Wert.
		sub esi, 4;				// berechne Zeiger auf neues Element
		mov ebx, [edi + 8];		// Lade Tail-Zeiger
		mov [ebx + 8], esi;		// Verbiege Nachfolgerzeiger des vormals letzten Elementes
		mov [edi + 8], esi;		// Verbiege tail-Zeiger
		mov [esi], edx;		// schreibe neues Datum
		mov [esi + 4], ebx;		// schreibe Vorgängerzeiger
		mov [esi + 8], esi;		// schreibe Nachfolgerzeiger (auf sich selbst)
		jmp ende;				// fertig

// Die Liste ist leer. Wir schreiben unser Elemt an die erste Stelle.
neu:	mov dword ptr [edi], 1;		// neue Anzahl ist 1
		mov eax, edi;				// Speicherpointer in eax
		add eax, 16;				// berechne Pointer auf head element
		mov dword ptr [edi+4], eax;	// neuer Head ist eax
		mov dword ptr [edi+8], eax;	// neuer Tail ist eax
		mov [eax], edx;				// schreibe Datum
		mov dword ptr [eax+4], eax;	// Vorgängerzeiger ist eax
		mov dword ptr [eax+8], eax;	// Nachfolgerzeiger ist eax
ende:
	}
}

/**************************** remove_nr **********************************/

// entfernt das mit nr spezifizierte Element
// Falls die Liste leer, wird nichts getan
// Falls nr<0 oder nr>Anzahl, wir nichts getan
void remove_nr(char* data_ptr, int nr)
{
	__asm
	{
		// Lade Variablen in die Register
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		mov ecx, nr;			// Lade nr in ecx
		cmp ecx, 1;				// Vergleiche nr mit 1
		jl ende;				// Falls nr<1, dann fertig
		cmp ecx, [edi]			// Vergleiche mit nr mit Anzahl
		jg ende;				// Falls nr>Anzahl, dann fertig

		// Überprüfe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche mit 1
		je head;				// Falls nr=1, dann Sonderfall
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl, dann Sonderfall

		// Laufe bis zum löschenden Element.
		// Laufe die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum nächsten Element
		loop laufe;				// Beende Schleife oder suche weiter

		// ebx zeigt jetzt auf das Element, das wir löschen wollen.
		cmp ebx, [edi+12];		// Vergleiche zu löschendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten lösche history

		// verbiege Zeiger, lösche Daten, verringere Anzahl
biege:	mov eax, [ebx + 4];		// lade Vorgängerzeiger
		mov edx, [ebx + 8];		// lade Nachfolgerzeiger
		mov [eax + 8], edx;		// verbiege Nachfolgerzeiger des Vorgängers
		mov [edx + 4], eax;		// verbiege Vorgängerzeiger des Nachfolgers
		mov dword ptr [ebx], 0;	// lösche Datum
		mov dword ptr [ebx+4], 0;	// lösche Vorgängerzeiger
		mov dword ptr [ebx+8], 0;	// lösche Nachfolgerzeiger
		dec dword ptr [edi];	// veringere Anzahl
		jmp ende;				// fertig.

		// Sonderfall: Zu löschendes Element ist erstes in der Liste
head:	push edi;				// Speicherzeiger als Parameter
		call remove_first;		// rufe remove_first
		pop edi;				// runterpoppen
		jmp ende;				// fertig

		// Sonderfall: Zu löschendes Element ist zweites in der Liste
tail:	push edi;				// Speicherzeiger als Parameter
		call remove_last;		// rufe remove_first
		pop edi;				// runterpoppen und fertig
ende:
	}
}

/******************************** remove_first *****************************/

// entfernt das erste Element aus der Liste, falls eines vorhanden.
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

void remove_last(char* data_ptr)
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
		mov eax, [edi + 8];		// Lade Tail-Zeiger
		cmp eax, [edi+12];		// Vergleiche zu löschendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten lösche history

		// Verbiege Zeiger, lösche Element und verringere Anzahl.
biege:	mov ebx, [eax + 4];		// lader Zeiger aufs vorletzte Element
		mov [edi + 8], ebx;		// Schreibe neuen Tail
		mov [ebx + 8], ebx;		// Verbiege Nachfolgerzeiger des neuen letzten Elementes auf sich selbst.
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

/******************************** get_nr *****************************/

// Suche in der Liste das Element nr und gebe das Datum zurück.
// Falls die Liste leer ist, so ist der Rückgabewert -1
// Falls nr<1 oder nr>Anzahl, so ist der Rückgabewert -1
int get_nr(char* data_ptr, int nr)
{
	__asm
	{
		// Lade Variablen in die Register
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		mov ecx, nr;			// Lade nr in ecx

		// Prüfe, ob Parameter im gültigen Bereich
		cmp ecx, 1;				// Vergleiche nr mit 1
		jl minus;				// Falls nr<1, dann fertig (mit -1)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		jg minus;				// Falls nr>Anzahl, dann fertig (mit -1)
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (mit -1)

		// Überprüfe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche mit 1
		je head;				// Falls nr=1, dann Sonderfall
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl, dann Sonderfall

		// Laufe bis zum gesuchten Element.
		// Laufe die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum nächsten Element
		loop laufe;				// Beende Schleife oder suche weiter

		// ebx zeigt jetzt auf das gesuchte Element
		mov [edi+12], ebx;		// Schreibe Zeiger in history
		mov eax, [ebx];			// lese Datum aus dem Speicher
		jmp ende;				// fertig

		// Sonderfall: auszugebendes Element ist das erste
head:	push edi;				// push Speicherzeiger als Parameter
		call get_first;			// rufe get_first
		pop edi;				// runterpoppen
		jmp ende;				// fertig.

		// Sonderfall: auszugebendes Element ist das letzte
tail:	push edi;				// push Speicherzeiger als Parameter
		call get_last;			// rufe get_last
		pop edi;				// runterpoppen
		jmp ende;
minus:	mov eax, -1;
ende:
	}
}

/*********************************** get_first *****************************************/

// gibt das erste Element der Liste zurück.
// Wenn die Liste leer ist, wird -1 zurückgegeben.
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
int get_last(char* data_ptr)
{
	__asm
	{
		// überprüfe zunächst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (mit -1)

		mov ebx, [edi+8];		// Lade Tail in ebx;
		mov [edi+12], ebx;		// Schreibe Tail in history
		mov eax, [ebx];			// Lese Datum aus dem Tail;
		jmp ende;				// fertig.

minus:	mov eax, -1;
ende:
	}
}

/************************************* get_next **************************************/

int get_next(char* data_ptr)
{
	__asm
	{
		// überprüfe zunächst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (minus eins)

		// Überprüfe, ob history = tail
		mov eax, [edi+12];		// Lade history-Zeiger
		cmp eax, [edi+8];		// Vergleiche history = tail
		je minus;				// Falls gleich, dann sind wir fertig (minus eins)

		// Überprüfe, ob history auf null zeigt.
		cmp dword ptr [edi+12], 0;	// Vergleiche history=0
		je head;				// Falls 0, dann gebe das erste Element zurück

		// Gebe Datum des Vorgängers zurück
		mov eax, [eax+8];		// ansonsten gehe zum Nachfolger
		mov [edi+12], eax;		// schreibe Zeiger in history
		mov eax, [eax];			// Lade Datum
		jmp ende;				// fertig

		// Sonderfall. Gebe Datum des resten Elementes zurück
head:	mov eax, [edi+4];		// Lade head Zeiger in eax
		mov [edi+12], eax;		// schreibe head-Zeiger in history
		mov eax, [eax];			// Lade Datum des ersten Elementes
		jmp ende;				// fertig

minus:	mov eax, -1;
ende:
	}
}

/************************************* get_previous **************************************/

int get_previous(char* data_ptr)
{
	__asm
	{
		// überprüfe zunächst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (minus eins)

		// Überprüfe, ob history = head
		mov eax, [edi+12];		// Lade history-Zeiger
		cmp eax, [edi+4];		// Vergleiche history = head
		je minus;				// Falls gleich, dann sind wir fertig (minus eins)

		// Überprüfe, ob history auf null zeigt.
		cmp dword ptr [edi+12], 0;	// Vergleiche history=0
		je tail;				// Falls 0, dann gebe das letzte Element zurück

		// Gebe Datum des Vorgängers zurück
		mov eax, [eax+4];		// ansonsten gehe zum Vorgänger
		mov [edi+12], eax;		// schreibe Zeiger in history
		mov eax, [eax];			// Lade Datum
		jmp ende;				// fertig

		// Sonderfall. Gebe Datum des letzten Elementes zurück
tail:	mov eax, [edi+8];		// Lade tail Zeiger in eax
		mov [edi+12], eax;		// schreibe tail-Zeiger in history
		mov eax, [eax];			// Lade Datum des letzten Elementes
		jmp ende;				// fertig

minus:	mov eax, -1;
ende:
	}
}

/****************************************************************************/
/************************ Aufgabe 2: Quicksort ******************************/
/****************************************************************************/

/******************************** quicksort ************************************/

void quicksort(int* vec, int len)
{
	__asm
	{
		// initialisiere Register
		mov ecx, len;			// lade Länge in ecx
		dec ecx					// Dekrement, damit Offset für Letztes Element
								// ecx ist rechter Zeiger
		jle ende;				// Falls Länge <= 1 sind wir fertig.
		mov edi, vec;			// Lade Basepointer
		mov ebx, -1;			// ebx ist linker Zeiger
		mov edx, [edi + 4*ecx];	// Pivotelement in edx

// bewege linken Zeiger nach rechts, bis er auf den rechten trifft
// oder ein Vektorelement findet, das größer als das Pivot-Element ist
links:	inc ebx;				// linker Zeiger nach rechts
		cmp ebx, ecx;			// hat linker Zeiger den rechten getroffen?
		jge pivot;				// Dann gehe und tausche Pivot.
		cmp [edi + 4*ebx], edx;	// Ansonsten prüfe den Eintrag im Vektor
		jle links;				// Falls er kleiner gleich dem Pivot, suche weiter

// bewege rechten Zeiger nach links, bis er auf den linken trifft
// oder ein Vektorelement findet, das kleiner gleich dem Pivot-Element ist
rechts:	dec ecx;				// linker Zeiger nach rechts
		cmp ebx, ecx;			// hat rechter Zeiger den linken getroffen?
		jge pivot;				// Dann gehe und tausche Pivot.
		cmp [edi + 4*ecx], edx;	// Ansonsen prüfe den Eintrag im Vektor.
		jg rechts;				// Falls er größer als das Pivot, suche weiter.

		// tausche zwei Einträge im Vektor und mache weiter mit der Zeigersuche
		mov eax, [edi + 4*ebx];	// lade altes linkes Element in Akkumulator
		xchg eax, [edi +4*ecx];	// vertausche Akkumulator mit rechtem Element
		mov [edi + 4*ebx], eax;	// schreibe neues linkes Element in den Speicher
		jmp links;				// suche neue Elemente zum Vertauschen.

// tauscht das Pivotelement in die Mitte, so dass es zwischen den beiden
// Hälften steht. Vorbereitung für rekursiven Aufruf.
pivot:	mov ecx, len;			// Lade Länge des Vektors in ecx
		dec ecx;				// dekrementiere Länge: Offset fürs Pivot-Element
		mov eax, [edi + 4*ebx];	// lade Element in den Akkumulator
		xchg eax, [edi +4*ecx];	// vertausche Akkumulator mit Pivot (ganz rechts)
		mov [edi + 4*ebx], eax;	// schreibe Akkumulator zwischen die beiden Hälften

		// erster rekursiver Aufruf mit der linken Hälfte
		push ebx;				// Position des Pivot ist Vektorlänge für neuen Aufruf
		push edi;				// alte Speicherzeiger ist neuer Speicherzeiger
		call quicksort;			// rekursiver Aufruf
		pop eax;				// runterpoppen,
		pop eax;				// Damit der Stack wieder stimmt

		// zweiter rekursiver Aufruf mit der rechten Hälfte
		inc ebx;				// dekrementiere neue linke Länge
		mov eax, len;			// Lade alte Länge in den Akkumulator
		sub eax, ebx;			// berechne neue rechte Länge
		push eax;				// Parameter auf den Stack
		shl ebx, 2;				// verringerte linke Länge * 4
        add edi, ebx;			// Addition mit altem Speicherzeiger
		push edi;				// neuen Speicherzeiger auf den Stack
		call quicksort;			// rekursiver Aufruf
		pop eax;				// runterpoppen,
		pop eax;				// Damit der Stack wieder stimmt
ende:
	}
}

/****************************************************************************/
/************************ Aufgabe 3: Pipelines ******************************/
/****************************************************************************/

/*
Gehen Sie im Folgenden von einer einfachen 7-stufigen Pipeline aus:
Befehl holen (IF)
Befehl dekodieren (ID)
Registerumbenennung (RR)
Operanden holen (OF)
Ausführung (EX)
Befehlsbestätigung (CO)
Rückspeichern (WB)

Weiterhin liegt eine reine Load/Store-Architektur ohne architekturelle
Beschleunigungsmaßnahmen (z.B. forwarding, reordering etc.) oder Hardware
zur Erkennung von Hemmnissen vor. Operanden können erst dann
aus Registern geholt werden, nachdem sie zurück gespeichert wurden.


- Wie viele Takte dauert die vollständige Abarbeitung von X:= A+B; A:= B-X
  ohne Optimierung?

  Gemeint ist, dass die Befehle nacheinander in die Pipeline gesteckt werden.
  Die Pipeline hat 7 Stufen, also braucht die Ausführung von 2 Befehlen
  eine Anzahl von 2*7 = 14 Takten.


- Auf wie viele Takte können die Operationen ohne Zusatzhardware optimiert werden?

  "Ohne Zusatzhardware" meint, dass keine der oben erwähnten Beschleunigungsmaßnahmen
  verwendet wird. Es wird einfach nur die Pipeline gefüllt, so gut es geht. Also,
  wie gut geht das? Es liegt eine "Datenabhängigkeit" vor. Der zweite Befehl liest aus
  dem Register X, welches vom ersten Befehl neu zugewiesen wird. Also muss der zweite
  Befehl mit dem Operand-fetch warten, bis der erste das Register in der Write-back-Phase
  geschrieben hat. Dies sind drei Wartezyklen. Nach der weiter unten gefundenen Formel
  Braucht die Ausführung beider Befehle somit n + 6 + 3 = 11 Phasen.
  Anmerkung: Mit Zusatzhardware könnte das Ergebnis der Berechnung des ersten Befehls
  vom Ausgang der Alu über eine eigene Leitung wieder an einen Eingang geführt werden.
  Diese Technik nennt man "Forwarding". Die 3 Wartezyklen wären so vermieden, da dass
  Ergebnis X gleich im nächsten Zyklus dem folgenden Befehl zur Verfügung stünde.


- Wer ist für die Auflösung von Pipelinekonflikten zuständig?

  Dies ist Abhängig von der Art des Prozessors. Bei einem reinen RISC-Processor,
  wo die Befehle ohne weitere Dekodierung in die Pipe gestopft werden,
  leistet diese Arbeit der Compiler, indem er zwischen die beiden in Konflikt
  stehenden Befehle eine entsprechende Anzahl von NOPs einfügt. Wenn man
  Assemblercode schreibt gibt es keinen Compiler, und der Programmierer muss
  die Arbeit selbst erledigen.
  Bei einem CISC-Prozessor werden die Befehle vom ISA-Level in den ersten Pipelinestufen
  in Mikrocode umgewandelt. Die Mikrobefehle werden der Reihe nach in die Pipeline
  gestopft. Für das Erkennen und Auflösen von Konflikten ist ein Teil des Steuerwerks
  zuständig, die "hazard detection unit".


- Wie viele Takte werden im Idealfall für die Abarbeitung von n Befehlen benötigt?

  Die Formel hierfür lautet n + k - 1, wobei k: Anzahl der Pipelinestufen
  und n: Anzahl der Befehle. Also in unserem Fall der 7-stufigen Pipeline: n + 6
*/