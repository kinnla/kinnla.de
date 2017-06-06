/**
 * L�sung zu Aufgabe 57 c)
 * Eine Menge, in der Elemente mehrfach enthalten sein d�rfen.
 */
class Multimenge {

  // maximale Anzahl verschiedener Elemente
  static final int maxL�nge = 100;

  // aktuelle Anzahl verschiedener Elemente
  int l�nge;

  // Array zum Speichern der Elemente
  int[] x;

  // Array zum Speichern der Vielfachheiten
  int[] a;

  /**
   * Erzeugt eine leere Multimenge.
   */
  Multimenge() {
    l�nge = 0;
    x = new int[maxL�nge];
    a = new int[maxL�nge];
  }

  /**
   * F�gt ein Element in die Multimenge ein.
   * Falls die Multimenge voll ist,
   * wird eine RuntimeException ausgel�st.
   */
  void einf�gen(int xx) {
    // Pr�fe, ob das Element bereits enthalten ist.
    for (int i=0; i<l�nge; ++i) {
      if (x[i] == xx) {
        // Fall 1: Das Element ist bereits enthalten.
        // wir erh�hen seine Vielfachheit.
        a[i]++;
        return;
      }
    }
    // Das Element ist nicht enthalten.
    if (l�nge == maxL�nge) {
      // Kein Platz frei. Fehler.
      throw new RuntimeException("Kann nicht einf�gen. Voll.");
    }
    // noch ein Platz frei. F�ge das neue Element
    // an das Ende des Arrays an und passe die l�nge an.
    x[l�nge] = xx;
    a[l�nge++] = 1;
  }

  /**
   * entfernt ein Element aus der Multimenge.
   * Wenn das Element nicht enthalten ist,
   * wird eine RuntimeException ausgel�st.
   */
  void entfernen(int xx) {
    // Pr�fe, ob das Element enthalten ist.
    for (int i=0; i<l�nge; ++i) {
      if (x[i] == xx) {
        // das Element ist enthalten. Verringere die Vielfachheit
        if (--a[i] == 0) {
          // letztes x wurde entfernt. Verschiebe nachfolgende x nach links.
          for (int j=i; j<l�nge-1; ++j) {
            a[j] = a[j+1];
            x[j] = x[j+1];
          }
          // Passe die L�nge an.
          l�nge--;
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
    for (int i=0; i<l�nge; ++i) {
      if (x[i] == xx) {
        // Element gefunden. Gebe seine Vielfachheit zur�ck.
        return a[i];
      }
    }
    // Element ist nicht enthalten. R�ckgabe 0.
    return 0;
  }

  /**
   * Nicht Teil der L�sung.
   * Testroutine zum Pr�fen der Invarianten.
   * Testet viel, aber nicht alles.
   * Das Verfahren ist gewisserma�en axiomatisch
   * (vgl. Axiome der algebraischen Definition).
   */
  public boolean test() {
    // Hilfsvariablen
    int zahl;
    int viel;
    int alteL�nge;

    // teste einf�gen
    zahl = (int) (Math.random() * 200);
    viel = wieviel(zahl);
    alteL�nge = l�nge;
    if (viel > 0) {
      // element bereits enthalten
      einf�gen(zahl);
      if (wieviel(zahl) != viel + 1 || l�nge != alteL�nge) {
        System.out.println("Fehler beim Einf�gen einer enthaltenen Zahl");
        return false;
      }
    } else {
      // element nicht enthalten
      if (l�nge == maxL�nge) {
        // einf�gen nicht m�glich. Fehler erwartet.
        try {
          einf�gen(zahl);
          System.out.println("Fehler beim Einf�gen in volles Array");
          return false;
        } catch (RuntimeException e) {
          // alles in Ordnung.
        }
      } else {
        // einf�gen m�glich.
        einf�gen(zahl);
        if (wieviel(zahl) != 1 || l�nge != alteL�nge + 1) {
          System.out.println("Fehler beim Einf�gen einer neuen Zahl");
          return false;
        }
      }
    }

    // teste entfernen
    zahl = (int) (Math.random() * 200);
    viel = wieviel(zahl);
    alteL�nge = l�nge;
    if (viel > 1) {
      // zahl mehrfach enthalten
      entfernen(zahl);
      if (wieviel(zahl) != viel - 1 || l�nge != alteL�nge) {
        System.out.println("Fehler beim Entfernen einer mehrfach enthaltenen Zahl");
        return false;
      }
    } else if (viel == 1) {
      // zahl einmal enthalten
      entfernen(zahl);
      if (wieviel(zahl) != 0 || l�nge != alteL�nge - 1) {
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
