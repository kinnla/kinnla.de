// Tabsize 4

// Tutorium Rechnerorganisation SS 2003
// Musterl�sung zum Aufgabenblatt 4 von Till Zoppke (zoppke@inf.fu-berlin.de)
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
		// �berschreibe gesamten Speicherbereich mit Nullen:
		// Anzahl Elemente ist null
		// Zeiger auf erstes Element ist null
		// Zeiger auf letztes Element ist null
		// Zeiger auf zuletzt gelesenes Element ist null
		// Keine Eintr�ge, also alles null

		mov ebx, data_ptr;		// lade Speicherzeiger in ebx
		sub ebx, 4;				// verringere Speicherzeiger um 4
								// (dann kann man die Schleife bequemer organisieren)
		mov ecx, 256;			// initialisiere ecx mit 256
								// ecx ist Offset f�rs Schreiben.
label:	mov DWORD PTR [ebx + 4*ecx], 0;	// schreibe null in den Speicher
		loop label;				// bis der Zeiger auf null kommt
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
		// Lade Variablen in die Register
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		mov edx, data;			// Lade Datum in edx
		mov ecx, nr;			// Lade nr in ecx

		// Pr�fe, ob Parameter im g�ltigen Bereich
		cmp dword ptr [edi],64;	// Vergleiche Anzahl = 64
		je ende;				// Falls = 64, dann kann nichts mehr eingef�gt werden, fertig.
		cmp ecx, 1;				// Vergleiche mit 1
		jl ende;				// Falls nr<1, dann fertig
		dec ecx;				// nr = nr-1 (zum Vergleichen)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		jg ende;				// Falls nr>Anzahl, dann fertig
		inc ecx;				// nr = nr+1 (wiederherstellen)

		// �berpr�fe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche nr mit 1
		je head;				// Falls nr=1, dann Sonderfall: erstes Element
		dec ecx;				// nr = nr+1 (zum Vergleichen)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl+1, dann Sonderfall: letztes Element
		inc ecx;				// nr = nr-1 (wiederherstellen)

		// suche feien Speicherplatz
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum n�chsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden.
		sub esi, 4;				// berechne Zeiger auf neues Element

		// F�ge das Element in der Mitte der Liste ein.
		// Laufe zun�chst die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx;
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum n�chsten Element
		loop laufe;				// Beende Schleife oder suche weiter

		// ebx zeigt jetzt auf das Element, vor dem wir einf�gen wollen.
		// verbiege Zeiger, ver�ndere Datum, erh�he Anzahl
		mov eax, [ebx + 4];		// lade Vorg�ngerzeiger
		mov [eax + 8], esi;		// verbiege Nachfolgerzeiger des Vorg�ngers
		mov [ebx + 4], esi;		// verbiege Vorg�ngerzeiger des Nachfolgers
		mov [esi], edx;			// schreibe Datum
		mov [esi + 4], eax;		// schreibe Vorg�ngerzeiger
		mov [esi + 8], ebx;		// schreibe Nachfolgerzeiger
		inc dword ptr [edi];	// erh�he Anzahl
		jmp ende;				// fertig.

		// Element wird an den Anfang eingef�gt.
		// Rufe Assemblerfunktion "add_first".
head:	push edx;				// Parameter data
		push edi;				// Parameter data_ptr
		call add_first;			// rekursiver Aufruf
		pop edi;				// runterpoppen
		pop edx;				// runterpoppen
		jmp ende;				// fertig

		// Element wird ans Ende eingef�gt.
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
		// Fallunterscheidung nach Anzahl der Eintr�ge
		mov edi, data_ptr;		// lade Speicherzeiger in edi
		mov edx, data;			// lade Datum in edx
		cmp DWORD PTR [edi],64;	// Pr�fe, ob schon 64 Eintr�ge vorhanden
		je ende;				// Falls ja, dann sind wir fertig.
		cmp DWORD PTR [edi],0;	// Pr�fe, ob schon ein Eintrag vorhanden
		je neu;					// falls nicht, gehe zu neu.

		// Anzahl der Eintr�ge ist 0 < Anzahl < 64
		// erh�he Anzahl und suche einen freien Speicherplatz
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum n�chsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden. Verbiege Zeiger und schreibe Wert.
		sub esi, 4;				// berechne Zeiger auf neues Element
		mov eax, [edi + 4]		// Lade Head-Zeiger
		mov [eax + 4], esi;		// verbiege Vorg�ngerzeiger des ehemaligen ersten Elementes
		mov [edi + 4], esi;		// Verbiege head-Zeiger
		mov [esi], edx;			// schreibe Datum
		mov [esi + 4], esi;		// schreibe Vorg�ngerzeiger (auf sich selbst)
		mov [esi + 8], eax;		// schreibe Nachfolgerzeiger (auf ehemaliges erstes)
		inc dword ptr [edi];	// erh�he Anzahl
		jmp ende;				// fertig

// Die Liste ist leer. Wir schreiben unser Elemt an die erste Stelle.
neu:	mov dword ptr [edi], 1;		// neue Anzahl ist 1
		mov eax, edi;				// Speicherpointer in eax
		add eax, 16;				// berechne Pointer auf erstes element
		mov dword ptr [edi+4], eax;	// neuer Head ist eax
		mov dword ptr [edi+8], eax;	// neuer Tail ist eax
		mov [eax], edx;				// schreibe Datum
		mov dword ptr [eax+4], eax;	// Vorg�ngerzeiger ist eax
		mov dword ptr [eax+8], eax;	// Nachfolgerzeiger ist eax
ende:
	}
}

/******************************* add_last ******************************/

// f�gt ein Element an das Ende der Liste an.
// Falls schon 64 Elemente vorhanden, ist nichts zu tun.
void add_last(char* data_ptr, int data)
{
	__asm
	{
		// Fallunterscheidung nach Anzahl der Eintr�ge
		mov edi, data_ptr;		// lade Speicherzeiger in edi
		mov edx, data;			// lade Datum in edx
		cmp DWORD PTR [edi],64;	// Pr�fe, ob schon 64 Eintr�ge vorhanden
		je ende;				// Falls ja, dann sind wir fertig.
		cmp DWORD PTR [edi],0;	// Pr�fe, ob schon ein Eintrag vorhanden
		je neu;					// falls nicht, gehe zu neu.

		// Anzahl der Eintr�ge ist 0 < Anzahl < 64
		// erh�he Anzahl und suche einen freien Speicherplatz
		inc dword ptr [edi]	// inkrementiere Anzahl
		mov esi, 8;				// Startwert ist 8 statt 20, wegen Schleifentechnik.
		add esi, edi;			// Addiere Speicherzeiger
suche:	add esi, 12;			// zum n�chsten Listenelement weiter
		cmp DWORD PTR [esi], 0;	// ist dort noch Platz?
		jne suche;				// falls nicht, suche weiter.

		// freier Speicherplatz gefunden. Verbiege Zeiger und schreibe Wert.
		sub esi, 4;				// berechne Zeiger auf neues Element
		mov ebx, [edi + 8];		// Lade Tail-Zeiger
		mov [ebx + 8], esi;		// Verbiege Nachfolgerzeiger des vormals letzten Elementes
		mov [edi + 8], esi;		// Verbiege tail-Zeiger
		mov [esi], edx;		// schreibe neues Datum
		mov [esi + 4], ebx;		// schreibe Vorg�ngerzeiger
		mov [esi + 8], esi;		// schreibe Nachfolgerzeiger (auf sich selbst)
		jmp ende;				// fertig

// Die Liste ist leer. Wir schreiben unser Elemt an die erste Stelle.
neu:	mov dword ptr [edi], 1;		// neue Anzahl ist 1
		mov eax, edi;				// Speicherpointer in eax
		add eax, 16;				// berechne Pointer auf head element
		mov dword ptr [edi+4], eax;	// neuer Head ist eax
		mov dword ptr [edi+8], eax;	// neuer Tail ist eax
		mov [eax], edx;				// schreibe Datum
		mov dword ptr [eax+4], eax;	// Vorg�ngerzeiger ist eax
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

		// �berpr�fe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche mit 1
		je head;				// Falls nr=1, dann Sonderfall
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl, dann Sonderfall

		// Laufe bis zum l�schenden Element.
		// Laufe die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum n�chsten Element
		loop laufe;				// Beende Schleife oder suche weiter

		// ebx zeigt jetzt auf das Element, das wir l�schen wollen.
		cmp ebx, [edi+12];		// Vergleiche zu l�schendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten l�sche history

		// verbiege Zeiger, l�sche Daten, verringere Anzahl
biege:	mov eax, [ebx + 4];		// lade Vorg�ngerzeiger
		mov edx, [ebx + 8];		// lade Nachfolgerzeiger
		mov [eax + 8], edx;		// verbiege Nachfolgerzeiger des Vorg�ngers
		mov [edx + 4], eax;		// verbiege Vorg�ngerzeiger des Nachfolgers
		mov dword ptr [ebx], 0;	// l�sche Datum
		mov dword ptr [ebx+4], 0;	// l�sche Vorg�ngerzeiger
		mov dword ptr [ebx+8], 0;	// l�sche Nachfolgerzeiger
		dec dword ptr [edi];	// veringere Anzahl
		jmp ende;				// fertig.

		// Sonderfall: Zu l�schendes Element ist erstes in der Liste
head:	push edi;				// Speicherzeiger als Parameter
		call remove_first;		// rufe remove_first
		pop edi;				// runterpoppen
		jmp ende;				// fertig

		// Sonderfall: Zu l�schendes Element ist zweites in der Liste
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

void remove_last(char* data_ptr)
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
		mov eax, [edi + 8];		// Lade Tail-Zeiger
		cmp eax, [edi+12];		// Vergleiche zu l�schendes Element mit history
		jne biege;				// falls ungleich, mache weiter.
		mov dword ptr [edi+12], 0;	// Ansonsten l�sche history

		// Verbiege Zeiger, l�sche Element und verringere Anzahl.
biege:	mov ebx, [eax + 4];		// lader Zeiger aufs vorletzte Element
		mov [edi + 8], ebx;		// Schreibe neuen Tail
		mov [ebx + 8], ebx;		// Verbiege Nachfolgerzeiger des neuen letzten Elementes auf sich selbst.
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

/******************************** get_nr *****************************/

// Suche in der Liste das Element nr und gebe das Datum zur�ck.
// Falls die Liste leer ist, so ist der R�ckgabewert -1
// Falls nr<1 oder nr>Anzahl, so ist der R�ckgabewert -1
int get_nr(char* data_ptr, int nr)
{
	__asm
	{
		// Lade Variablen in die Register
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		mov ecx, nr;			// Lade nr in ecx

		// Pr�fe, ob Parameter im g�ltigen Bereich
		cmp ecx, 1;				// Vergleiche nr mit 1
		jl minus;				// Falls nr<1, dann fertig (mit -1)
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		jg minus;				// Falls nr>Anzahl, dann fertig (mit -1)
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (mit -1)

		// �berpr�fe, ob ein Sonderfall vorliegt
		cmp ecx, 1;				// Vergleiche mit 1
		je head;				// Falls nr=1, dann Sonderfall
		cmp ecx, [edi]			// Vergleiche mit Anzahl
		je tail;				// Falls nr=Anzahl, dann Sonderfall

		// Laufe bis zum gesuchten Element.
		// Laufe die Nachfolgerzeiger nr-mal durch.
		mov ebx, edi;			// Lade Datenzeiger in ebx
		sub ebx, 4;				// Trick, damit man gleich den head trifft.
laufe:	mov ebx, [ebx + 8];		// Gehe zum n�chsten Element
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

// gibt das erste Element der Liste zur�ck.
// Wenn die Liste leer ist, wird -1 zur�ckgegeben.
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
int get_last(char* data_ptr)
{
	__asm
	{
		// �berpr�fe zun�chst, ob die Liste leer ist
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
		// �berpr�fe zun�chst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (minus eins)

		// �berpr�fe, ob history = tail
		mov eax, [edi+12];		// Lade history-Zeiger
		cmp eax, [edi+8];		// Vergleiche history = tail
		je minus;				// Falls gleich, dann sind wir fertig (minus eins)

		// �berpr�fe, ob history auf null zeigt.
		cmp dword ptr [edi+12], 0;	// Vergleiche history=0
		je head;				// Falls 0, dann gebe das erste Element zur�ck

		// Gebe Datum des Vorg�ngers zur�ck
		mov eax, [eax+8];		// ansonsten gehe zum Nachfolger
		mov [edi+12], eax;		// schreibe Zeiger in history
		mov eax, [eax];			// Lade Datum
		jmp ende;				// fertig

		// Sonderfall. Gebe Datum des resten Elementes zur�ck
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
		// �berpr�fe zun�chst, ob die Liste leer ist
		mov edi, data_ptr;		// Lade Speicherzeiger in edi
		cmp dword ptr [edi], 0;	// Vergleiche Anzahl=0
		je minus;				// Falls 0, dann sind wir fertig (minus eins)

		// �berpr�fe, ob history = head
		mov eax, [edi+12];		// Lade history-Zeiger
		cmp eax, [edi+4];		// Vergleiche history = head
		je minus;				// Falls gleich, dann sind wir fertig (minus eins)

		// �berpr�fe, ob history auf null zeigt.
		cmp dword ptr [edi+12], 0;	// Vergleiche history=0
		je tail;				// Falls 0, dann gebe das letzte Element zur�ck

		// Gebe Datum des Vorg�ngers zur�ck
		mov eax, [eax+4];		// ansonsten gehe zum Vorg�nger
		mov [edi+12], eax;		// schreibe Zeiger in history
		mov eax, [eax];			// Lade Datum
		jmp ende;				// fertig

		// Sonderfall. Gebe Datum des letzten Elementes zur�ck
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
		mov ecx, len;			// lade L�nge in ecx
		dec ecx					// Dekrement, damit Offset f�r Letztes Element
								// ecx ist rechter Zeiger
		jle ende;				// Falls L�nge <= 1 sind wir fertig.
		mov edi, vec;			// Lade Basepointer
		mov ebx, -1;			// ebx ist linker Zeiger
		mov edx, [edi + 4*ecx];	// Pivotelement in edx

// bewege linken Zeiger nach rechts, bis er auf den rechten trifft
// oder ein Vektorelement findet, das gr��er als das Pivot-Element ist
links:	inc ebx;				// linker Zeiger nach rechts
		cmp ebx, ecx;			// hat linker Zeiger den rechten getroffen?
		jge pivot;				// Dann gehe und tausche Pivot.
		cmp [edi + 4*ebx], edx;	// Ansonsten pr�fe den Eintrag im Vektor
		jle links;				// Falls er kleiner gleich dem Pivot, suche weiter

// bewege rechten Zeiger nach links, bis er auf den linken trifft
// oder ein Vektorelement findet, das kleiner gleich dem Pivot-Element ist
rechts:	dec ecx;				// linker Zeiger nach rechts
		cmp ebx, ecx;			// hat rechter Zeiger den linken getroffen?
		jge pivot;				// Dann gehe und tausche Pivot.
		cmp [edi + 4*ecx], edx;	// Ansonsen pr�fe den Eintrag im Vektor.
		jg rechts;				// Falls er gr��er als das Pivot, suche weiter.

		// tausche zwei Eintr�ge im Vektor und mache weiter mit der Zeigersuche
		mov eax, [edi + 4*ebx];	// lade altes linkes Element in Akkumulator
		xchg eax, [edi +4*ecx];	// vertausche Akkumulator mit rechtem Element
		mov [edi + 4*ebx], eax;	// schreibe neues linkes Element in den Speicher
		jmp links;				// suche neue Elemente zum Vertauschen.

// tauscht das Pivotelement in die Mitte, so dass es zwischen den beiden
// H�lften steht. Vorbereitung f�r rekursiven Aufruf.
pivot:	mov ecx, len;			// Lade L�nge des Vektors in ecx
		dec ecx;				// dekrementiere L�nge: Offset f�rs Pivot-Element
		mov eax, [edi + 4*ebx];	// lade Element in den Akkumulator
		xchg eax, [edi +4*ecx];	// vertausche Akkumulator mit Pivot (ganz rechts)
		mov [edi + 4*ebx], eax;	// schreibe Akkumulator zwischen die beiden H�lften

		// erster rekursiver Aufruf mit der linken H�lfte
		push ebx;				// Position des Pivot ist Vektorl�nge f�r neuen Aufruf
		push edi;				// alte Speicherzeiger ist neuer Speicherzeiger
		call quicksort;			// rekursiver Aufruf
		pop eax;				// runterpoppen,
		pop eax;				// Damit der Stack wieder stimmt

		// zweiter rekursiver Aufruf mit der rechten H�lfte
		inc ebx;				// dekrementiere neue linke L�nge
		mov eax, len;			// Lade alte L�nge in den Akkumulator
		sub eax, ebx;			// berechne neue rechte L�nge
		push eax;				// Parameter auf den Stack
		shl ebx, 2;				// verringerte linke L�nge * 4
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
Ausf�hrung (EX)
Befehlsbest�tigung (CO)
R�ckspeichern (WB)

Weiterhin liegt eine reine Load/Store-Architektur ohne architekturelle
Beschleunigungsma�nahmen (z.B. forwarding, reordering etc.) oder Hardware
zur Erkennung von Hemmnissen vor. Operanden k�nnen erst dann
aus Registern geholt werden, nachdem sie zur�ck gespeichert wurden.


- Wie viele Takte dauert die vollst�ndige Abarbeitung von X:= A+B; A:= B-X
  ohne Optimierung?

  Gemeint ist, dass die Befehle nacheinander in die Pipeline gesteckt werden.
  Die Pipeline hat 7 Stufen, also braucht die Ausf�hrung von 2 Befehlen
  eine Anzahl von 2*7 = 14 Takten.


- Auf wie viele Takte k�nnen die Operationen ohne Zusatzhardware optimiert werden?

  "Ohne Zusatzhardware" meint, dass keine der oben erw�hnten Beschleunigungsma�nahmen
  verwendet wird. Es wird einfach nur die Pipeline gef�llt, so gut es geht. Also,
  wie gut geht das? Es liegt eine "Datenabh�ngigkeit" vor. Der zweite Befehl liest aus
  dem Register X, welches vom ersten Befehl neu zugewiesen wird. Also muss der zweite
  Befehl mit dem Operand-fetch warten, bis der erste das Register in der Write-back-Phase
  geschrieben hat. Dies sind drei Wartezyklen. Nach der weiter unten gefundenen Formel
  Braucht die Ausf�hrung beider Befehle somit n + 6 + 3 = 11 Phasen.
  Anmerkung: Mit Zusatzhardware k�nnte das Ergebnis der Berechnung des ersten Befehls
  vom Ausgang der Alu �ber eine eigene Leitung wieder an einen Eingang gef�hrt werden.
  Diese Technik nennt man "Forwarding". Die 3 Wartezyklen w�ren so vermieden, da dass
  Ergebnis X gleich im n�chsten Zyklus dem folgenden Befehl zur Verf�gung st�nde.


- Wer ist f�r die Aufl�sung von Pipelinekonflikten zust�ndig?

  Dies ist Abh�ngig von der Art des Prozessors. Bei einem reinen RISC-Processor,
  wo die Befehle ohne weitere Dekodierung in die Pipe gestopft werden,
  leistet diese Arbeit der Compiler, indem er zwischen die beiden in Konflikt
  stehenden Befehle eine entsprechende Anzahl von NOPs einf�gt. Wenn man
  Assemblercode schreibt gibt es keinen Compiler, und der Programmierer muss
  die Arbeit selbst erledigen.
  Bei einem CISC-Prozessor werden die Befehle vom ISA-Level in den ersten Pipelinestufen
  in Mikrocode umgewandelt. Die Mikrobefehle werden der Reihe nach in die Pipeline
  gestopft. F�r das Erkennen und Aufl�sen von Konflikten ist ein Teil des Steuerwerks
  zust�ndig, die "hazard detection unit".


- Wie viele Takte werden im Idealfall f�r die Abarbeitung von n Befehlen ben�tigt?

  Die Formel hierf�r lautet n + k - 1, wobei k: Anzahl der Pipelinestufen
  und n: Anzahl der Befehle. Also in unserem Fall der 7-stufigen Pipeline: n + 6
*/