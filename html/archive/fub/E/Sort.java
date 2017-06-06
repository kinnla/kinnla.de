public class Sort {

	static void insertionSort(int[] a) {
		int x;
		for (int i = 1; i<a.length; i++) {
			x = a[i];
			// Sortiere x zwischen a[0..i-1] ein
			// Bestimme zunächst die richtige Position j:
			int j=i;
			while (j>0 && a[j-1]>x) {
				j--;
			}
			// Nun verschiebe einen Teil des Feldes
			// und füge x an dieser Stelle ein:
			for (int k = i-1; k>=j; k--) {
				a[k+1]=a[k];
			}
			a[j]=x;
		}
	}

	static void selectionSort(int[] a) {
		for (int i=0; i<a.length - 1; ++i) {
			int k = i; // k ist Index des vorläufiges Minimums
			int j = i;
			while (j < a.length - 1) {
				if (a[++j] < a[k]) {
					k = j; // neues Minimum gefunden
				}
			}
			// vertausche Minimum mit erstem Element des unsortierten Teils
			int x = a[k];
			a[k] = a[i];
			a[i] = x;
		}
	}

	private static String intArrayToString(int[] a) {
		// Fall 1: Array der Länge 0
		if (a.length == 0) {
			return "[]";
		}
		// Fall 2: Array der Länge > 0
		String s = "[" + a[0];
		for (int i=1; i<a.length; ++i) {
			s = s + ", " + a[i];
		}
		s += "]";
		return s;
	}

	private static void initRandom(int[] a, int min, int max) {
		for (int i=0; i<a.length; ++i) {
			a[i] = min + (int) (Math.random() * (max - min + 1));
		}
	}

	public static void main(String[] args) {
		int[] a = new int[0]; // leeres Array
		int[] b = new int[20]; // zufällig initialisiertes Array

		// InsertionSort
		insertionSort(a);
		System.out.println("Insertion: " + intArrayToString(a));
		initRandom(b, 0, 9);
		insertionSort(b);
		System.out.println("Insertion: " + intArrayToString(b));

		// selectionSort
		selectionSort(a);
		System.out.println("Selection: " + intArrayToString(a));
		initRandom(b, 0, 9);
		selectionSort(b);
		System.out.println("Selection: " + intArrayToString(b));
	}
}
