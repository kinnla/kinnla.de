public class Aufgabe35 {
    public static void main(String argv[]){
	Tree.main(new String[0]);
/*
	System.out.println();
	Tree23 b23 = new Tree23();
	if(argv.length >= 1) {
	    System.out.println("Eintragen jedes Zeichens des 1.Arguments \n" 
			       + argv[0] + "\nunter seiner Differenz zum Zeichen 'a'"
			       + " in einen 2-3-Baum :");
	    for(int i=0 ; i < argv[0].length() ; i++) {
		CharItem ci = new CharItem( argv[0].charAt(i) );
		b23.add(ci.key() ,ci ,true);
		System.out.println( "Eintrag (" + ci.key() + ",'" 
				    + ci.c +"') hinzugefuegt :\n" + b23);
	    }
	    //
	    try {
		System.out.print("Hoehe " + b23.height() + ", "
				 + b23.size() + " Eintraege, Minimum =" 
				 + b23.minimum() + " Maximum =" + b23.maximum()
				 + " : \n" + "[" );
	    
		int alleKeys[] = b23.toIntArray();
		for(int n=0 ; n < alleKeys.length-1 ; n++) 
		    System.out.print( alleKeys[n] + ", " );
		System.out.println( alleKeys[alleKeys.length-1] + "]" );
		int key = 1;
		System.out.println( "Baum enthaelt "
				    + (b23.contains(key)?"":"KEINEN")
				    + " Eintrag mit Schluessel " + key + "."); 
		key = -10 ;
		System.out.println( "Baum enthaelt "
				    + (b23.contains(key)?"":"KEINEN ")
				    + "Eintrag mit Schluessel " + key + "."); 
		key = 7; // -12;
		if(b23.remove(key)) {
		    System.out.println(key + " wurde entfernt !\n" + b23); 
		} else  System.out.println(key + " nicht existent !\n" + b23); 

	        for(int i=0 ; i < argv[0].length() ; i++) {
		    CharItem ci = new CharItem( argv[0].charAt(i) );
		    if(b23.remove(ci.key())) {
		       System.out.println(ci.key() +" wurde entfernt !\n" +b23); 
		    } else System.out.println(ci.key()
					      +" nicht existent !\n" +b23); 
		}
	    }
	    catch (Tree23.EmptyTreeException leer) {  
		System.err.println(leer.getMessage());
	    }
	}
*/
    }
}

// Die eigentliche zu einem Schluessel "key()" in einer Struktur abgelegte
// Information. In 2-3-Baum nur in den Blaettern.
interface Item {
    public int key();
}

// zum Ablegen von Zeichen in 2-3-Baum
class CharItem implements Item {
    char c; CharItem(char c) { this.c = c;}
    public int key() { return (c - 'a'); }
    public String toString() {  return "" + c; }
}

// Knoten von Baeumen
// Mutter von Blaettern und inneren Knoten
abstract class Node {
    static boolean debug = true;
    int k1;  // Schluessel-Wert
    // Einfuegen eines Paares aus Schluessel <key> und Zusatz-Information <item>
    // ,existiert schon ein Eintrag mit dem Schluessel <key> und es ist 
    // <ueberschreiben> == true wird alte durch neue Zusatzinformation ersetzt,
    //  sonst - bei <ueberschreiben> == false - die alte nicht ersetzt.
    // Zurueckgeliefert wird "null" wenn die 2-3-Baum-Struktur oberhalb nicht
    // veraendert werden muss, sonst ein zusaetzlich einzufuegender Teilbaum.
    abstract Node insert(int key , Item item , boolean ueberschreiben);

    // Entfernen eines Eintrags mit <key> aus Teilbaum
    abstract int remove(int key ,Leaf23 neuesMinimum);

    // Rueckgabe des Eintrags zu "key" wenn dieser existiert (Genauer
    // eines Verweises auf das eine Blatt mit Schluessel <key>)
    // , sonst null :
    abstract Leaf23 search(int key); 
    abstract int minimum();
    abstract int maximum();
    abstract int size();
    abstract int toIntArray(int []intArray, int intArray_pos );
    abstract String intoString(int deep);

    // Hilfsfunktion: String aus <n> Blanks / Leerzeichen :
    public static String blanks(int n) {
	if(n <= 0) return new String("");
	char bl[] = new char[n];
	for(int i=bl.length; i-- > 0 ; bl[i] = ' ');  
	return new String(bl);
    }
}

// Blatt (-Knoten) fuer 2-3-Baum : 
// Paar aus Schluessel "k1" und dazu eingetragener Information "item"
class Leaf23 extends Node {  
    Item item;
    Leaf23(int key, Item item) { k1 = key;   this.item = item; }
    // Gibt es schon einen Eintrag mit <key> wird immer null geliefert
    //  der alte, d.h. das alte durch das neue "item" ersetzt, falls 
    //  ueberschreiben == true, falls ueberschreiben == false wird nichts 
    //  veraendert.
    // Ist der Eintrag neu, so wird der kleinere im Original-Blatt eingetragen
    // und ein VERWEIS AUF den groesseren - also ein NOCH NICHT EINGETRAGENES
    // BLATT - GELIEFERT.
    Node insert(int key , Item item , boolean ueberschreiben) {
	if(key == k1) { if(ueberschreiben) this.item = item; return null; } 
	// Ab hier ist key # k1
	Leaf23 neuL = new Leaf23(key,item);
	if(key < k1) { // Neuer Eintrag ist kleiner -> vertauschen
	    neuL.k1 = k1;   neuL.item = this.item;
	    k1 = key;    this.item = item;
	}
	return neuL;  
    }

    int remove(int key ,Leaf23 neuesMinimum) {
	if(key == k1) return -1; // key gefunden , Blatt muss entfernt werden
	return  0;  // key existiert nicht
    }

    Leaf23 search(int key) { if(key == k1 ) return this; return null; }
    String intoString(int deep){
	return Node.blanks(3*deep) + "-| " + k1 
	    + ((item!=null)?(" :\t" + item):("")) + "\n";
    }

    int minimum() { return k1; }
    int maximum() { return k1; }
    int size() { return 1; }

    // zurueckgegeben wird die (naechste) Position zum Eintragen des
    // naechsten (Blatt-)Schluessels in das "intArray[]"
    int toIntArray(int intArray[], int intArray_pos ){
	if(intArray_pos < intArray.length)
	    intArray[intArray_pos++] = k1;
	return intArray_pos;
    }
}

// innerer Knoten fuer 2-3-Baum:
// 2 Schluessel k1,k2 und 3 Unterbaeume t0, t1 und t2
// Es muessen 2 (dann sind t0 != null und t1 != null
//       oder 3 Eintraege (dann sind alle t0,t1,t2 != null)
//       existieren.                                       
// Grundsaetzlich gilt, dass alle Schluessel/Eintraege
// im Unterbaum tx kleiner als k(x+1) sind und
// kx das Minimum - den kleinsten eingetragenen Schluessel
// im Teilbaum tx bezeichnet.
class Node23 extends Node {
    int k2;  // 2. Schluessel neben k1
    Node t0, t1, t2;
    // Anzahl Eintraege / Unterbaeume des Knotens
    int eintraege() {
	if(t2 != null) return 3;
	if(t1 != null) return 2;  
	if(t0 != null) return 1;
	return 0; 
    }
    // der zu <key> passende Zweig zum weitersuchen oder einfuegen.
    // <eintraege> gibt dabei die tatsaechliche Anzahl Unterbaeume
    // des Knotens an.
    int zweig(int key, int eintraege) { 
	switch(eintraege) {
	case 3 : if(key >= k2) return 2;
	case 2 : if(key >= k1) return 1;
	default: return 0;
	}
    }

    Leaf23 search(int key) { 
	switch(zweig(key, eintraege())) {
	case 2: return t2.search(key);
	case 1: return t1.search(key);
	    // case 0: if(t0 != null) return t0.search(key);
	case 0: return t0.search(key);
	default: return null;
	}
    }

    Node insert(int key , Item item, boolean ueberschreiben) {
	// Finde richtigen Zweig zum Einfuegen
	int eintraege = eintraege();
	int zweig = zweig(key, eintraege);
	Node tneu = null; // Neuer hier einzufuegender Teilbaum 
	switch(zweig) {
	case 2: tneu = t2.insert(key, item, ueberschreiben);  break;
	case 1: tneu = t1.insert(key, item, ueberschreiben);  break;
	case 0: tneu = t0.insert(key, item, ueberschreiben);  break;
	}
	if( tneu == null ) return null; // kein weiterer Knoten einzufuegen
	// Neuer Unterbaum/Zweig tneu != null muss hier eingefuegt werden :
	int tneu_min = tneu.k1;
	// wenn tneu ein Blatt, dann ist dessen Minimum sein Key tneu.k1,
	// sonst steht Minimum des Teilbaums "tneu" in unbenutztem Feld "k2"
	if(tneu instanceof Node23)  tneu_min = ((Node23) tneu).k2;

	Node23 neuerKnoten = null;
	if(eintraege == 3) {	// schon 3 Unterbaeume, kein Platz mehr also 
	    neuerKnoten = new Node23(); // aufteilen auf diesen u. "neuerKnoten"

	    // 1. die 2 groessten Teilbaume geordnet in "neuerKnoten" eintragen
	    // 2. den kleinsten Eintrag/Minim. aus neuerKnoten.t0 in unbenutztem
	    //    ( bisher neuerKnoten.t2 == null ) Feld "k2" eintragen also
	    //    neuerKnoten.k2 = neuerKnoten.Minimum();
	    // 3. "neuerKnoten" an Elternknoten - oder Dummy-Knoten - zum 
	    //    Einfuegen weiterreichen  ( return neuerKnoten; ).
	    switch(zweig) {  
	    case 2: // tneu als 4. Teilbaum von 4
		neuerKnoten.t1 = tneu;   neuerKnoten.k1 = tneu_min; //tneu.k2; 
		neuerKnoten.t0 = t2;     neuerKnoten.k2 = k2;
		t2 = null;
		return neuerKnoten;
	    case 1: // tneu als 3. Teilbaum von 4
		neuerKnoten.t1 = t2;     neuerKnoten.k1 = k2;
		t2 = null;
		neuerKnoten.t0 = tneu;   neuerKnoten.k2 = tneu_min; //tneu.k2;
		return neuerKnoten;
	    case 0: // tneu als 2. Teilbaum von 4
		neuerKnoten.t1 = t2;     neuerKnoten.k1 = k2;
		neuerKnoten.t0 = t1;     neuerKnoten.k2 = k1;
		t2 = t1 = null;
		// Da uebersichtlicher : HIER das Einfuegen von tneu als t1
		t1 = tneu;  k1 = tneu_min;
		return neuerKnoten;
		// Einfuegen von "tneu" spaeter - statt der letzten 2 Zeilen
		// wuerde reichen:
	        // eintraege = 1;  // Noch ein Eintrag im alten Knoten uebrig
	    }
	}
	// Hier gibt es genau (1 oder) 2 Eintraege oder Unterbaeume im Knoten :
	// Eintragen des neuen Knotens/Teilbaums als neuen Zweig Nr: <zweig>+1
	if(zweig == 1) { t2 = tneu;  k2 = tneu_min; 
	} else { // zweig == 0  -> Eintragen des neuen Teilbaums als t1
	    //if(eintraege == 2) 
	    { t2 = t1; k2 = k1; } // t1 nach t2 schieben
	    t1 = tneu;  k1 = tneu_min;  
	}
	return neuerKnoten;
    }

   /* Entfernen eines Eintrags mit <key> aus Teilbaum. Zurueckgegeben werden im
      Verlauf der rekursiven Funktions-Aufrufe die Werte
      -1 Blatt auf unterster Knotenebene ist zu entfernen
       0 Key existiert nicht im Baum, konnte also NICHT GELOESCHT werden
       1 key GELOESCHT + NICHTS mehr ZU TUN
       2 Nur NEUES MINIMUM in Teilbaum, evt. noch Anpassen von ElternSchluesseln
       3 UNTERLAUF (nur noch ein Eintrag) in Unterbaum
       4 UNTERLAUF + NEUES MINIMUM in Unterbaum
      und bei den Rueckgaben "2" und "4" ueber das Feld neuesMinimum.k1 das
      neue (groessere) Minimale Element des veraenderten Teilbaums (das alte
      Minimum wurde dann entfernt). Eine Referenz auf ein nicht benutztes,
      existierendes Objekt vom Typ Leaf23 muss dafuer vom Aufrufer mitgeliefert
      werden.
   */
   int remove(int key ,Leaf23 neuesMinimum) {
	if(neuesMinimum == null) return 1;
	// Finde richtigen Zweig mit evt. zu entfernendem Eintrag :
	int eintraege = eintraege();
	int zweig = zweig(key, eintraege);
	int res=0; // Bisheriges Resultat des Loesch-Versuchs
	// "remove(key,neuesMinimum)" auf dem richtigen Zweig :
	switch(zweig) {
	case 2: res = t2.remove(key, neuesMinimum);  break;
	case 1: res = t1.remove(key, neuesMinimum);  break;
	case 0: res = t0.remove(key, neuesMinimum);  break;
	}
	// Was ist nach "remove(key,neuesMinimum)" auf Zweig noch zu tun ?
	if(res == 0) return 0;  // key existiert nicht - nichts geloescht
	if(res == -1)           // Zweig ist das zu loeschende Blatt mit <key>:
	    return entferneBlatt(zweig, eintraege, neuesMinimum);
	if(res == 1) return 1;  // key GELOESCHT + NICHTS mehr ZU TUN
	if(res == 2) { // Nur NEUES MINIMUM in Teilbaum <zweig>
	    switch(zweig) {
	    case 2: k2 = neuesMinimum.k1; return 1; // nichts weiter zu tun
	    case 1: k1 = neuesMinimum.k1; return 1; // nichts weiter zu tun
	    case 0: return 2;  // evt. noch Anpassen von ElternSchluesseln
	    }
	}
	if(res == 3 || res == 4) { // Unterlauf in Kind <zweig>
	    Node23 t[] = new Node23[3];  // nur um Typecasts zu umgehen
	    t[0] = ((Node23) t0);  t[1] = ((Node23) t1);  t[2] = ((Node23) t2);

	    // Versuche Nachbarn zum Abgeben eines von 3 Kindern zu finden:
	    switch(zweig) {
	    case 2: if( t[1].eintraege() == 3) {
		if(res == 4) t[2].k1 = neuesMinimum.k1;
		else         t[2].k1 = k2;
		k2 = t[1].k2;   // t1.t2 nach t2 verschieben  
		t[2].t1 = t[2].t0; t[2].t0 = t[1].t2; t[1].t2 = null;
		return 1;  // res = 1; // nichts weiter zu tun
	    }
		break;
	    case 1: if( t[0].eintraege() == 3) {  // t0.t3 nach t1.t0
		if(res == 4)   t[1].k1 = neuesMinimum.k1;
		else           t[1].k1 = k1;
		t[1].t1 =t[1].t0;  t[1].t0 =t[0].t2;  t[0].t2 =null;
		k1 = t[0].k2;
		return 1;   // nichts weiter zu tun
	    } // else 
		if( eintraege == 3 && t[2].eintraege() == 3) {
		    // t2.t0 nach t1.t1
		    if(res == 4)   k1 = neuesMinimum.k1;
		    t[1].t1 = t[2].t0;  t[2].t0 = t[2].t1;
		    t[2].t1 = t[2].t2;  t[2].t2 = null;
		    t[1].k1 = k2;   k2 = t[2].k1;  t[2].k1 = t[2].k2;  
		    return 1;   // nichts weiter zu tun
		}
		break;
	    case 0: if( t[1].eintraege() == 3) {  // t1.t1 nach t0.t1
		t[0].t1 = t[1].t0;  t[1].t0 = t[1].t1;
		t[1].t1 = t[1].t2;  t[1].t2 = null;
		t[0].k1 = k1;  k1 = t[1].k1;  t[1].k1 = t[1].k2;
		if(res == 4) return 2;   // immer noch Neues Minimum (in t0)
		return 1;   // nichts weiter zu tun
	    }
		break;
	    }  // Die direkten Nachbarn haben selbst nur 2 Kinder - bleibt nur 
	       // das Verschmelzen mit einem Nachbarn zu Knoten mit 3 Kindern :
	    switch(zweig) {
	    case 2: // Verschmelzen von t1 und t2 :
		if(res == 4)   t[1].k2 = neuesMinimum.k1;
		else           t[1].k2 = k2;
		t[1].t2 = t[2].t0;
		t2 = null;
		return 1;   // nichts weiter zu tun
	    case 1: if(eintraege == 3) { // Verschmelzen von t1 und t2 :
		t[1].t1 = t[2].t0;   t[1].t2 = t[2].t1;
		t[1].k1 = k2;        t[1].k2 = t[2].k1;
		if(res == 4) k1 = neuesMinimum.k1;
		t2 = null;
		return 1;   // nichts weiter zu tun
	    } // else eintraege == 2 also UNTERLAUF nach Verschmelzen
		// Verschmelzen von t0 und t1 :
		if(res == 4)   t[0].k2 = neuesMinimum.k1;
		else           t[0].k2 = k1;
		t[0].t2 = t[1].t0;
		t1 = null;
		return 3;  // UNTERLAUF 
	    case 0: // Verschmelzen von t0 und t1 :
		t[0].t1 = t[1].t0;  t[0].t2 = t[1].t1;
		t[0].k1 = k1; t[0].k2 = t[1].k1;
		k1 = k2;  t1 = t2;  t2 = null;
		if(eintraege == 2)
		    if(res == 3) return 3; // UNTERLAUF
		    else        return 4; // UNTERLAUF + NEUES MINIMUM
		// also war eintraege == 3 und damit KEIN UNTERLAUF 
		else if(res == 3) return 1;  // Nichts weiter zu tun
		else              return 2;  //           NEUES MINIMUM
	    }
	}
	return 0;
    }

    // Entfernen eines Blattes in einem inneren Knoten und Rekonstruieren
    // Rueckgabewerte aus { 1, 2, 3, 4 } und Argumente wie bei remove()
    int entferneBlatt(int zweig, int eintraege, Leaf23 neuesMinimum) {
	int res = 1;  // Rueckgabewert 
	switch(zweig) {  // Loeschen des Blattes :
	case 2: t2 = null;  return 1;  // Nichts weiter zu tun
	case 1: if(eintraege == 3) {
	    t1 = t2;  t2 = null;   k1 = k2;
	    return 1;
	} else { // UNTERLAUF: Entfernen des groesseren von 2 Eintraegen
	    t1 = null;
	    return 3;  // UNTERLAUF
	}
	case 0:     neuesMinimum.k1 = k1;
	    t0 = t1;   
	    t1 = t2;  t2 = null;   k1 = k2;  
	    if(eintraege == 3)
		return 2;  // Nur NEUES MINIMUM in Teilbaum
	    else // der Kleinere von 2 Eintraegen entfernt :
		return 4;  // UNTERLAUF + NEUES MINIMUM
	}
	return 1;  // Sollte hier nie hinkommen
    }

    /*
      String intoString(int deep){
      String accu = new String(); 
      String blanks = blanks(3*deep);

      if(t0 instanceof Leaf23) accu += "\n"; // Trenner fuer unterste Ebene
      if(t0 != null) accu +=                      t0.intoString(deep+1);
      if(t1 != null) accu += blanks + k1 + "\n" + t1.intoString(deep+1);
      if(t2 != null) accu += blanks(3*deep-1) + "," + k2 + "\n"
      + t2.intoString(deep+1);
      return accu;
      }
    */  // Besser Ausgabe in umgekehrter Inorder :
    String intoString(int deep){
	String accu = new String(); 
	String blanks = blanks(3*deep);

	if(t2 != null) accu += t2.intoString(deep+1)
			   + blanks(3*deep-1) + "," + k2 + "\n";
	if(t1 != null) accu += t1.intoString(deep+1) + blanks + k1 + "\n";
	if(t0 != null) accu +=                      t0.intoString(deep+1);
	if(t0 instanceof Leaf23) accu += "\n"; // Trenner fuer unterste Ebene
	return accu;
    }

    int minimum() { return t0.minimum(); }
    int maximum() { if(t2 == null) return t1.maximum();  return t2.maximum(); }
    int size() {  if(t2 == null) return t0.size() + t1.size();
    return t0.size() + t1.size() + t2.size();
    }

    int toIntArray(int intArray[], int intArray_pos ){
	intArray_pos =t0. toIntArray(intArray ,intArray_pos);
	intArray_pos =t1. toIntArray(intArray ,intArray_pos);
	if(t2 != null)
	    return t2.toIntArray(intArray ,intArray_pos);
	return intArray_pos;
    }
}

class Tree23 extends Tree {  // Verwaltung der Wurzel eines 2-3-Baums
    public boolean remove(int key){ 
	if(t0 == null) return false;

	// Rueckgabe-Container fuer zusaetzliche Rueckgabe von Node.remove().
	// Zurueckgegeben wird das neu Minimum eines Teilbaum nach entfernen
	// des alten.
	Leaf23 neuesMinimum = new Leaf23(0 , null); // Wert 0 bedeutungslos
	int res = t0.remove(key ,neuesMinimum);
	if(res == 0) return false;  // key existiert nicht im Baum
	                            // konnte also nicht geloescht werden.
	if(res == 1) return true;   // key wurde geloescht
	// Einziges / Letztes Blatt aus sonst leerem Baum entfernen :
	if(res == -1  /* &&  t0 instanceof Leaf23 */ ) {
	    t0 = null;
	    h = -1;
	    return true; 
	} 
	// if(res == 2) // neues Minimum eines Teilbaums evt. key's nachtragen
	if(res == 3 || res == 4) {  // Unterlauf in WurzeKnoten t0, entfernen
	    // Teilbaum - hier also t0 - hat Unterlauf d.h. nur 1 Eintrag
	    t0 = ((Node23) t0).t0;
	    h--;  // Hoehe des Baums um 1 verkleinert
	}
	return true;
    }

    int h; // Hoehe des Baums 
    Node t0;    // Verweis auf den eigentlichen Baum :
    // == null wenn der Baum LEER
    // ,Verweis auf EINZELNES BLATT bei einem Eintrag,
    // ,Verweis auf die WURZEL des Baums sonst 
    // So koennen "normale" innere Knoten "Node23" immer
    // 2 bis 3 Eintraege haben, ohne das dort Sonderfaelle
    // beruecksichtigt werden muessen.

    public Tree23(){ h = -1; t0 = null; /* Baum ist noch LEER */ }

    public int size() { if(t0 == null) return 0;  return t0.size(); } 
    public boolean isEmpty() { return t0==null; }  // { return h == -1; }
    public int height() { return h; }
    public boolean contains(int key) {
	if(t0 == null) return false;
	return t0.search(key) != null; 
    }

    public int minimum() throws EmptyTreeException {
        if(t0==null) throw new EmptyTreeException();
	// "Baum leer : Minimum existiert nicht");
	return t0.minimum(); 
    }

    public int maximum() throws EmptyTreeException {
        if(t0==null) throw new EmptyTreeException();
	// "Baum leer : Maximum existiert nicht");
	return t0.maximum(); 
    }

    // Einfuegen von Schluessel key (ohne Zusatz-Information)
    public void add(int key) { add(key ,null ,true /* altes ueberschreiben*/ );}

    // Einfuegen eines Paares aus Schluessel <key> und Zusatz-Information <item>
    // unter Erhalt der 2-3-Baum - Eigenschaften.
    // Falls <ueberschreiben> == true, wird bei wiederholtem Einfuegen eines
    // Schluessels alte durch neue Zusatz-Information ersetzt, sonst behalten.
    public void add(int key , Item item, boolean ueberschreiben) {
	if(t0 == null) { // 1. Blatt / Eintrag einfuegen
	    t0 = new Leaf23(key, item);
	    h++;	// Hoehe von -1 auf 0 erhoehen
	    return; 
	} // Schon mindestens ein Eintrag / Blatt im Baum 
	
	Node wurzelHaelfte = t0.insert(key, item, ueberschreiben); 
	if(wurzelHaelfte != null) { // bisherige Wurzel des Baumes war voll und 
	    // wurde gespalten -> WURZEL WIEDERHERSTELLEN:
            // wenn "wurzelHaelfte" ein Blatt, ist dessen Minimum sein Key "k1", 
            // sonst steht Minimum des Teilbaums "wurzelHaelfte" temporaer in 
	    // unbenutztem Feld "k2"
	    int wurzelHaelfte_min;
            if(wurzelHaelfte instanceof Node23)  
		wurzelHaelfte_min = ((Node23) wurzelHaelfte).k2;
	    else
	        wurzelHaelfte_min = ((Leaf23) wurzelHaelfte).k1;
	    Node23 neueWurzel = new Node23();	 
	    neueWurzel.k1 = wurzelHaelfte_min; 
	    neueWurzel.t0 = t0;  neueWurzel.t1 = wurzelHaelfte;
	    t0 = neueWurzel;
	    h++; // Hoehe des Baums um 1 erhoehen
	}
    }

    public void clear() { h = -1; t0 = null; }

    public int[] toIntArray(){
	if(t0 == null) return new int[0];
        int intArr[] = new int[size()];
	t0.toIntArray(intArr, 0);
	return intArr;
    }

    public String toString(){ 
	if(t0 == null) return "2-3-Baum : Hoehe " + h + " ist leerer Baum\n";
	return "2-3-Baum : Hoehe " + h + "\n" + t0.intoString(1); 
    }
}


class MyTree extends Tree23 {}  // Anpassung an Tree.main() s.u.
class TwoThree2 extends Tree23  {}  // Anpassung an Tree.main() s.u.
// Bis auf die naechste Zeile nachfolgend "Tree.java" von Till Zoppke :
//package tree;

/**
 * Klasse Tree für einen 2-3-Baum, der ganze Zahlen speichert. 
 * 
 * Diese Klasse enthält:
 * - sondierende Funktionen, welche Auskunft über den aktuellen Status des
 *   Objektes geben (das sieht viel aus, ist aber wenig).
 * - Methoden zum einfügen und entfernen (sieht wenig aus, ist aber viel).
 * - Ausgabefunktionen. "toIntArray" ist hilfreich zum Testen, da die Zahlen-
 *   folge streng aufsteigend sortiert sein muss, was sich leicht prüfen lässt.
 *   "toString" gibt eine String-Repräsentation des Baumes zurück. Wie die 
 *   aussieht, ist nicht so wichtig. Aber zum Debuggen ist die Funktion 
 *   unverzichtbar.
 * - Testfunktionen. Nach Bedarf erweitern. Hilfreich wäre z.B. eine Funktion
 *   "checkNodes", die prüft, ob jedes Kind eines Knotens diesen Knoten auch
 *   zum Elter hat.
 * - Die innere Klasse "EmptyTreeException". 
 * 
 * Was ihr machen soll:
 * - Schreibt eine Klasse (z.B. "MyTree"), welche alle abstrakten Methoden
 *   implementiert.
 * - Entwerft eine weitere Klasse (z.B. "Node") für die inneren Knoten des 
 *   Baumes. Die Blattknoten kann man als Integer-Objekte speichern. Oder
 *   man speichert sie gleich in den inneren Knoten der vorletzten Ebene als 
 *   int-Werte. 
 * - Setzt den Testlauf für nichtleere Bäume fort. 
 * 
 * Was zu beachten ist:
 * - Die 2-3 Bäume der Vorlesung speichern alle Schlüssel in den Blättern (dort,
 *   aber nicht nur dort). Achtung: Im Internet finden man meistens eine andere 
 *   Variante. Diese wird nicht akzeptiert.
 * - Implementiert das Einfügen und Entfernen so, wie es in der Vorlesung
 *   vorgestellt wurde.
 * - Verschwendet nicht systematisch Speicherplatz! Verwendet für die 
 *   2 oder 3 Kinder eines inneren Knotens keinen Vector, verwendet für die
 *   Blattknoten nicht die gleichen Objekte wie für die inneren Knoten.
 * - Euer Baum sollte mit (theoretisch) beliebig großen Datenmengen klarkommen,
 *   also keine fest eingebaute Obergrenze haben.
 */
/*public*/ abstract class Tree {

	///////////////////////// sondierende Funktionen ///////////////////////////

	// bestimmt die Größe des Baumes. Ein leerer Baum hat Größe 0.
	public abstract int size();

	// ist der Baum leer?
	public abstract boolean isEmpty();

	// bestimmt die Höhe des Baumes. Ein leerer Baum hat Höhe -1.
	public abstract int height();

	// ist der Schlüssel enthalten?
	public abstract boolean contains(int key);

	// Gibt das Minimum zurück. 
	// Falls der Baum leer ist, wird eine Exception ausgelöst.
	public abstract int minimum() throws EmptyTreeException;

	// Gibt das Maximum zurück.
	// Falls der Baum leer ist, wird eine Exception ausgelöst.
	public abstract int maximum() throws EmptyTreeException;

	////////////////////////// Aufbauen und abbauen ////////////////////////////

	// fügt einen Schlüssel in den Baum ein
	public abstract void add(int key);

	// löscht den angegebenen Schlüssel aus dem Baum.
	// Falls der Schlüssel gefunden (und gelöscht) wird, ist der Rückgabewert
	// true, ansonsten false.
	public abstract boolean remove(int key);

	// leert den Baum
	public abstract void clear();

	///////////////////////////////// Ausgabe //////////////////////////////////

	// gibt die im Baum enthaltenen Schlüssel in aufsteigender Reihenfolge
	// zurück.
	public abstract int[] toIntArray();

	// gibt eine Zeichenkette zurück, die den Baum darstellt
	public abstract String toString();

	///////////////////////////////// Testen ///////////////////////////////////

	public static void main(String[] args) {
		// Größe 0
		Tree tree = new TwoThree2();
		System.out.println("Größe des leeren Baumes: " + tree.size());
		System.out.println("Ist der leere Baum leer?: " + tree.isEmpty());
		System.out.println("Höhe des leeren Baumes: " + tree.height());
		System.out.println(
			"Ist Schlüssel 1 im leeren Baum?: " + tree.contains(1));
		try {
			System.out.print("Minimum im leeren Baum: ");
			System.out.println(tree.minimum() + " No exception! FEHLER!");
		} catch (EmptyTreeException e) {
			System.out.println("exception.");
		}
		try {
			System.out.print("Maximum im leeren Baum: ");
			System.out.println(tree.maximum() + " No exception! FEHLER!");
		} catch (EmptyTreeException e) {
			System.out.println("exception.");
		}
		System.out.println("Entfernen im leeren Baum: " + tree.remove(2));
		System.out.println(
			"Zahlenfolge im leeren Baum: "
				+ intArrayToString(tree.toIntArray()));
		System.out.println("Stringausgabe vom leeren Baum: " + tree);
		System.out.println("Füge Schlüssel 1 hinzu:");
		tree.add(1);
		System.out.println("Schlüssel 1 hinzugefügt.");
		// Größe 1
		System.out.println("Größe des 1-Baumes: " + tree.size());
		System.out.println("Ist der 1-Baum leer?: " + tree.isEmpty());
		System.out.println("Höhe des 1-Baumes: " + tree.height());
		System.out.println("Ist Schlüssel 1 im 1-Baum?: " + tree.contains(1));
		System.out.println("Ist Schlüssel 2 im 1-Baum?: " + tree.contains(2));
		System.out.println("Ist Schlüssel 0 im 1-Baum?: " + tree.contains(0));
		try {
			System.out.print("Minimum im 1-Baum: ");
			System.out.println(tree.minimum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		try {
			System.out.print("Maximum im 1-Baum: ");
			System.out.println(tree.maximum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		System.out.println(
			"Zahlenfolge im 1-Baum: " + intArrayToString(tree.toIntArray()));
		System.out.println("Stringausgabe des 1-Baum: " + tree);
		System.out.println("Füge vorhandenen Schlüssel 1 erneut hinzu.");
		tree.add(1);
		System.out.println("Schlüssel 1 hinzugefügt.");
		System.out.println("Stringausgabe des 1-Baum: " + tree);
		System.out.println("Entferne 2 aus dem 1-Baum: " + tree.remove(2));
		System.out.println("Entferne 0 aus dem 1-Baum: " + tree.remove(0));
		System.out.println("Entferne 1 aus dem 1-Baum: " + tree.remove(1));
		System.out.println("Stringausgabe des leeren Baumes: " + tree);
		System.out.println("Entferne 1 noch einmal: " + tree.remove(1));
		System.out.println("Und noch einmal Stringausgabe: " + tree);
		System.out.println(
			"Und die Zahlenfolge: " + intArrayToString(tree.toIntArray()));
		System.out.println("Füge die Schlüssel 1 und 2 hinzu: ");
		tree.add(1);
		tree.add(2);
		System.out.println("Schlüssel 1 und 2 hinzugefügt.");
		// Größe 2
		System.out.println("Größe des 1-2-Baumes: " + tree.size());
		System.out.println("Ist der 1-2-Baum leer?: " + tree.isEmpty());
		System.out.println("Höhe des 1-2-Baumes: " + tree.height());
		System.out.println("Ist Schlüssel 1 im 1-2-Baum?: " + tree.contains(1));
		System.out.println("Ist Schlüssel 2 im 1-2-Baum?: " + tree.contains(2));
		System.out.println("Ist Schlüssel 0 im 1-2-Baum?: " + tree.contains(0));
		System.out.println("Ist Schlüssel 3 im 1-2-Baum?: " + tree.contains(3));
		try {
			System.out.print("Minimum im 1-2-Baum: ");
			System.out.println(tree.minimum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		try {
			System.out.print("Maximum im 1-2-Baum: ");
			System.out.println(tree.maximum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		System.out.println("Stringausgabe vom 1-2-Baum: " + tree);
		System.out.println(
			"Zahlenfolge im 1-2-Baum: " + intArrayToString(tree.toIntArray()));
		System.out.println("Füge vorhandene Schlüssel 1 und 2 erneut hinzu.");
		tree.add(1);
		tree.add(2);
		System.out.println("Schlüssel 1 und 2 hinzugefügt.");
		System.out.println("Stringausgabe des 1-2-Baum: " + tree);
		System.out.println("Entferne 3 aus dem 1-2-Baum: " + tree.remove(3));
		System.out.println("Entferne 0 aus dem 1-2-Baum: " + tree.remove(0));
		System.out.println("Entferne 1 aus dem 1-2-Baum: " + tree.remove(1));
		System.out.println("Stringausgabe des 2-Baumes: " + tree);
		System.out.println("Entferne 1 noch einmal: " + tree.remove(1));
		System.out.println("Und noch einmal Stringausgabe: " + tree);
		System.out.println("Entferne 2 aus dem 1-Baum: " + tree.remove(2));
		System.out.println("Stringausgabe leeren Baumes: " + tree);
		System.out.println(
			"Und die Zahlenfolge: " + intArrayToString(tree.toIntArray()));
		System.out.println("Füge die Schlüssel 1, 2 und 3 hinzu: ");
		tree.add(1);
		tree.add(3);
		tree.add(2);
		System.out.println("Schlüssel 1, 2 und 3 hinzugefügt.");
		// Größe 3
		System.out.println("Größe des 1-2-3-Baumes: " + tree.size());
		System.out.println("Ist der 1-2-3-Baum leer?: " + tree.isEmpty());
		System.out.println("Höhe des 1-2-3-Baumes: " + tree.height());
		System.out.println(
			"Ist Schlüssel 1 im 1-2-3-Baum?: " + tree.contains(1));
		System.out.println(
			"Ist Schlüssel 2 im 1-2-3-Baum?: " + tree.contains(2));
		System.out.println(
			"Ist Schlüssel 3 im 1-2-3-Baum?: " + tree.contains(3));
		System.out.println(
			"Ist Schlüssel 0 im 1-2-3-Baum?: " + tree.contains(0));
		System.out.println(
			"Ist Schlüssel 4 im 1-2-3-Baum?: " + tree.contains(4));
		try {
			System.out.print("Minimum im 1-2-3-Baum: ");
			System.out.println(tree.minimum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		try {
			System.out.print("Maximum im 1-2-3-Baum: ");
			System.out.println(tree.maximum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		System.out.println("Stringausgabe vom 1-2-3-Baum: " + tree);
		System.out.println(
			"Zahlenfolge im 1-2-3-Baum: "
				+ intArrayToString(tree.toIntArray()));
		System.out.println(
			"Füge vorhandene Schlüssel 1, 2 und 2 erneut hinzu.");
		tree.add(1);
		tree.add(2);
		tree.add(3);
		System.out.println("Schlüssel 1, 2 und 3 hinzugefügt.");
		System.out.println("Stringausgabe des 1-2-3-Baum: " + tree);
		System.out.println("Entferne 4 aus dem 1-Baum: " + tree.remove(4));
		System.out.println("Entferne 0 aus dem 1-Baum: " + tree.remove(0));
		System.out.println("Entferne 1 aus dem 1-Baum: " + tree.remove(1));
		System.out.println("Stringausgabe des 2-3-Baumes: " + tree);
		System.out.println("Entferne 1 noch einmal: " + tree.remove(1));
		System.out.println("Und noch einmal Stringausgabe: " + tree);
		System.out.println("Entferne 2 aus dem 2-3-Baum: " + tree.remove(2));
		System.out.println("Stringausgabe 3-Baumes: " + tree);
		System.out.println("leere den 3-Baum.");
		tree.clear();
		System.out.println("Stringausgabe des leeren Baumes: " + tree);
		System.out.println(
			"Und die Zahlenfolge: " + intArrayToString(tree.toIntArray()));
		System.out.println("Füge die Schlüssel 1, 2, 3 und 4 hinzu: ");
		tree.add(1);
		tree.add(3);
		tree.add(2);
		tree.add(4);
		System.out.println("Schlüssel 1, 2, 3 und 4 hinzugefügt.");
		// Größe 4
		System.out.println("Größe des 1-2-3-4-Baumes: " + tree.size());
		System.out.println("Ist der 1-2-3-4-Baum leer?: " + tree.isEmpty());
		System.out.println("Höhe des 1-2-3-4-Baumes: " + tree.height());
		System.out.println(
			"Ist Schlüssel 1 im 1-2-3-4-Baum?: " + tree.contains(1));
		System.out.println(
			"Ist Schlüssel 2 im 1-2-3-4-Baum?: " + tree.contains(2));
		System.out.println(
			"Ist Schlüssel 3 im 1-2-3-4-Baum?: " + tree.contains(3));
		System.out.println(
			"Ist Schlüssel 0 im 1-2-3-4-Baum?: " + tree.contains(0));
		System.out.println(
			"Ist Schlüssel 4 im 1-2-3-4-Baum?: " + tree.contains(4));
		System.out.println(
			"Ist Schlüssel 5 im 1-2-3-4-Baum?: " + tree.contains(5));
		try {
			System.out.print("Minimum im 1-2-3-4-Baum: ");
			System.out.println(tree.minimum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		try {
			System.out.print("Maximum im 1-2-3-4-Baum: ");
			System.out.println(tree.maximum());
		} catch (EmptyTreeException e) {
			System.out.println("exception. Fehler!");
		}
		System.out.println("Stringausgabe vom 1-2-3-4-Baum: " + tree);
		System.out.println(
			"Zahlenfolge im 1-2-3-4-Baum: "
				+ intArrayToString(tree.toIntArray()));
		System.out.println(
			"Füge vorhandene Schlüssel 1, 2, 3 und 4 erneut hinzu.");
		tree.add(1);
		tree.add(2);
		tree.add(4);
		tree.add(3);
		System.out.println("Schlüssel 1, 2, 3 und 4 hinzugefügt.");
		System.out.println("Stringausgabe des 1-2-3-4-Baum: " + tree);
		System.out.println("Entferne 4 aus dem 1-Baum: " + tree.remove(4));
		System.out.println("Entferne 0 aus dem 1-Baum: " + tree.remove(0));
		System.out.println("Entferne 1 aus dem 1-Baum: " + tree.remove(1));
		System.out.println("Stringausgabe des 2-3-Baumes: " + tree);
		System.out.println("Entferne 1 noch einmal: " + tree.remove(1));
		System.out.println("Und noch einmal Stringausgabe: " + tree);
		System.out.println("Entferne 2 aus dem 2-3-Baum: " + tree.remove(2));
		System.out.println("Stringausgabe 3-Baumes: " + tree);
		System.out.println("leere den 3-Baum.");
		tree.clear();
		System.out.println("Stringausgabe des leeren Baumes: " + tree);
		System.out.println(
			"Und die Zahlenfolge: " + intArrayToString(tree.toIntArray()));
		// Test durch Zufall.
		// Das haut den stärksten Baum um.
		System.out.println("+++++++++++ Test durch Zufall ++++++++++++");
		boolean error = false;
		for (int i = 0; i < 100 && !error; ++i) {
			tree = new TwoThree2();
			for (int j = 0; j < 1000 && !error; ++j) {
				int number =(int) (Math.random() * 100);
				//System.out.println(tree);
				//System.out.println("adding "+ number);
				tree.add(number);
				int[] keys = tree.toIntArray();
				if (!isSorted(keys)) {
					System.out.println("Unsortierte Zahlenfolge nach Einfügen!");
					System.out.println(intArrayToString(keys));
					System.out.println(tree);
					error = true;
				} else {
					tree.remove((int) (Math.random() * 100));
					keys = tree.toIntArray();
					if (!isSorted(keys)) {
						System.out.println("Unsortierte Zahlenfolge nach Entfernen!");
						System.out.println(intArrayToString(keys));
						System.out.println(tree);
						error = true;
					}
				}
			}
			//System.out.println(tree);
		}
		if (!error){
			System.out.println("Zufall ok.");
		}
		System.out.println("++++++++++++++ Testlauf beendet ++++++++++++++");
	}

	// Ausgabe eines integer-Arrays
	public static String intArrayToString(int[] array) {
		if (array.length == 0) {
			return "[]";
		}
		String s = "[" + array[0];
		for (int i = 1; i < array.length; ++i) {
			s = s + "," + array[i];
		}
		return s += "]";
	}

	// überprüft, ob ein integer-Array streng aufsteigend sortiert
	// Für so was wünscht man sich dann wieder haskell...
	public static boolean isSorted(int[] array) {
		for (int i = 1; i < array.length; ++i) {
			if (array[i] <= array[i - 1]) {
				return false;
			}
		}
		return true;
	}

	////////////////////// innere Klasse EmptyTreeException ////////////////////

	class EmptyTreeException extends Exception {
		public EmptyTreeException() {
		}
	}
}



