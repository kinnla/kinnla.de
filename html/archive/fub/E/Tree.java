package tree;

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
public abstract class Tree {

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

	// überprüft, ob ein integer-Array streng aufsteigend sortiert ist
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
