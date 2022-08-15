package singleton;

/**
 * Created by mattie on 4/5/16.
 */
public class DemoSingleton {
    private static DemoSingleton single = null;

    public static synchronized DemoSingleton getInstance() {
        if (single == null) {
            single = new DemoSingleton();
        }

        return single;
    }

    public static void main(String[] args) {
        System.out.println("Singleton Demo firing up!");

        DemoSingleton first = getInstance();
        first.increment();
        System.out.println("first value is: " + Integer.toString(first.total()));

        DemoSingleton second = getInstance();
        second.increment();
        System.out.println("second call value is: " + Integer.toString(second.total()));

        DemoSingleton third = getInstance();
        third.increment();
        System.out.println("third call value is: " + Integer.toString(third.total()));

        System.out.println("End of Demo");
    }

    private int myInt = 0;

    public void increment() {
        myInt++;
    }
    public int total() {
        return myInt;
    }

    private DemoSingleton() {}
}
