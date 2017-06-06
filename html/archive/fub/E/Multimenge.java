/**
 * Lösung zu Aufgabe 57 c)
 * Eine Menge, in der Elemente mehrfach enthalten sein dürfen.
 */
class Multimenge {

  // maximale Anzahl verschiedener Elemente
  static final int maxLänge = 100;

  // aktuelle Anzahl verschiedener Elemente
  int länge;

  // Array zum Speichern der Elemente
  int[] x;

  // Array zum Speichern der Vielfachheiten
  int[] a;

  /**
   * Erzeugt eine leere Multimenge.
   */
  Multimenge() {
    länge = 0;
    x = new int[maxLänge];
    a = new int[maxLänge];
  }

  /**
   * Fügt ein Element in die Multimenge ein.
   * Falls die Multimenge voll ist,
   * wird eine RuntimeException ausgelöst.
   */
  void einfügen(int xx) {
    // Prüfe, ob das Element bereits enthalten ist.
    for (int i=0; i<länge; ++i) {
      if (x[i] == xx) {
        // Fall 1: Das Element ist bereits enthalten.
        // wir erhöhen seine Vielfachheit.
        a[i]++;
        return;
      }
    }
    // Das Element ist nicht enthalten.
    if (länge == maxLänge) {
      // Kein Platz frei. Fehler.
      throw new RuntimeException("Kann nicht einfügen. Voll.");
    }
    // noch ein Platz frei. Füge das neue Element
    // an das Ende des Arrays an und passe die länge an.
    x[länge] = xx;
    a[länge++] = 1;
  }

  /**
   * entfernt ein Element aus der Multimenge.
   * Wenn das Element nicht enthalten ist,
   * wird eine RuntimeException ausgelöst.
   */
  void entfernen(int xx) {
    // Prüfe, ob das Element enthalten ist.
    for (int i=0; i<länge; ++i) {
      if (x[i] == xx) {
        // das Element ist enthalten. Verringere die Vielfachheit
        if (--a[i] == 0) {
          // letztes x wurde entfernt. Verschiebe nachfolgende x nach links.
          for (int j=i; j<länge-1; ++j) {
            a[j] = a[j+1];
            x[j] = x[j+1];
          }
          // Passe die Länge an.
          länge--;
        }
        return;
      }
    }
    // Element ist nicht enthalten. Fehler.
    throw new RuntimeException("Element nicht enthalten.");
  }

  /**
   * Bestimmt, wie oft das Element enthalten ist.
   */
  int wieviel (int xx) {
    // suche das Element.
    for (int i=0; i<länge; ++i) {
      if (x[i] == xx) {
        // Element gefunden. Gebe seine Vielfachheit zurück.
        return a[i];
      }
    }
    // Element ist nicht enthalten. Rückgabe 0.
    return 0;
  }

  /**
   * Nicht Teil der Lösung.
   * Testroutine zum Prüfen der Invarianten.
   * Testet viel, aber nicht alles.
   * Das Verfahren ist gewissermaßen axiomatisch
   * (vgl. Axiome der algebraischen Definition).
   */
  public boolean test() {
    // Hilfsvariablen
    int zahl;
    int viel;
    int alteLänge;

    // teste einfügen
    zahl = (int) (Math.random() * 200);
    viel = wieviel(zahl);
    alteLänge = länge;
    if (viel > 0) {
      // element bereits enthalten
      einfügen(zahl);
      if (wieviel(zahl) != viel + 1 || länge != alteLänge) {
        System.out.println("Fehler beim Einfügen einer enthaltenen Zahl");
        return false;
      }
    } else {
      // element nicht enthalten
      if (länge == maxLänge) {
        // einfügen nicht möglich. Fehler erwartet.
        try {
          einfügen(zahl);
          System.out.println("Fehler beim Einfügen in volles Array");
          return false;
        } catch (RuntimeException e) {
          // alles in Ordnung.
        }
      } else {
        // einfügen möglich.
        einfügen(zahl);
        if (wieviel(zahl) != 1 || länge != alteLänge + 1) {
          System.out.println("Fehler beim Einfügen einer neuen Zahl");
          return false;
        }
      }
    }

    // teste entfernen
    zahl = (int) (Math.random() * 200);
    viel = wieviel(zahl);
    alteLänge = länge;
    if (viel > 1) {
      // zahl mehrfach enthalten
      entfernen(zahl);
      if (wieviel(zahl) != viel - 1 || länge != alteLänge) {
        System.out.println("Fehler beim Entfernen einer mehrfach enthaltenen Zahl");
        return false;
      }
    } else if (viel == 1) {
      // zahl einmal enthalten
      entfernen(zahl);
      if (wieviel(zahl) != 0 || länge != alteLänge - 1) {
        System.out.println("Fehler beim Entfernen einer einfach enthaltenen Zahl");
        return false;
      }
    } else {
      // zahl nicht enthalten. Fehler erwartet.
      try {
        entfernen(zahl);
        System.out.println("Fehler beim Entfernen einer nicht enthaltenen Zahl");
        return false;
      } catch (RuntimeException e) {
        // alles in Ordnung.
      }
    }

    // test erfolgreich
    return true;
  }

  public static void main(String[] args) {
    Multimenge mm = new Multimenge();
    for (int i=0; i<10000; ++i) {
      if (!mm.test()) {
        System.exit(1);
      }
    }
    System.out.println("Test erfolgreich.");
    System.exit(0);
  }
}
