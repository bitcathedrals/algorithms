package threaded;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import java.util.concurrent.ArrayBlockingQueue;

import java.util.concurrent.atomic.AtomicBoolean;

import java.util.concurrent.Callable;

public class FutureTask<T> implements Future<T>, Callable<Exception> {
 
    // our asynchronous task
    Callable<T> task = null;
    
    // the values we will cache
    Exception error;
    private T result = null;
    
    // the lock that protects the cached values from being accessed before the async task is complete.
    private AtomicBoolean isDone;
    
    
    // the Worker is a single thread that takes asynchronous tasks from a BlockingQueue, executes them,
    // and then waits for the next task.
    
    private static class Worker implements Runnable  {

        static final int QueueBacklog = 5; 
        private static ArrayBlockingQueue<Callable<Exception>> runQueue = null;
        
        private Thread workerThread = null;
        private AtomicBoolean goFlag = null;
        
        public Worker() {
            runQueue = new ArrayBlockingQueue<Callable<Exception>>(QueueBacklog);

            goFlag = new AtomicBoolean(true);
            
            workerThread = new Thread(this);
            workerThread.start();
        }
           
        public void doWork(Callable<Exception> async) {
            try {
                runQueue.put(async);
            } catch (InterruptedException e) {
                e.toString();
            }
        }
        
        public synchronized void waitForWork() {
            
            boolean interrupted = false;
            
            do {
                try {
                    this.wait();
                    interrupted = false;
                } catch (InterruptedException e) {
                    interrupted = true;
                }
            } while(interrupted == true);
        }
        
        @Override
        public synchronized void run() {
           while(goFlag.get() == true) {

               // wait for work to do.
               Callable<Exception> work = null;
               
               try {
                   work = runQueue.take();
               } catch (InterruptedException e) {
                   continue;     
               }    
              
               // do the work.
               try {
                   work.call();
               } catch (Exception e) {               
                   e.printStackTrace();
               }
               
               // wake up all the people waiting on the workManager so they can check if their
               // work is done.
               this.notifyAll();
           }
        }
        
        public void destroy() {
            goFlag.set(false);
        }
        
    };
    
    // The static worker object
    static private Worker workManager = null;

    public static void createWorker() {
        workManager = new Worker();
    }
    
    public static void destroyWorker() {
        if(workManager != null ) {
            workManager.destroy();
        }
    }
    
    public FutureTask(Callable<T> work) {
        // set isDone to false for starters.
        isDone = new AtomicBoolean(false);
    
        // set the handler.
        this.task = work;
        
        // Add ourselves to the workManager. This may block if the QueueBacklog is exceeded.
        workManager.doWork(this);
    }
    
    public void waitForWork() {
        // waitForWork will return every time some work is completed by the workManager.
        // It may or may not be our work that was completed so continue to wait until
        // our isDone flag comes up.
        
        while(! isDone.get() ) {
            System.out.println("I started waiting");
            workManager.waitForWork();
            System.out.println("I stopped waiting");
        }
    }
    
    @Override
    public boolean cancel(boolean arg0) {
        return false;
    }

    @Override
    public T get() {
        waitForWork();
        return result;
    }

    @Override
    public T get(long arg0, TimeUnit arg1) throws InterruptedException,
                                                  ExecutionException, 
                                                  TimeoutException {
        waitForWork();
        return result;
    }

    @Override
    public boolean isCancelled() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isDone() {
      return isDone.get();
    }
    
    
    public Exception call() {

        try {
            result = task.call();
   
        } catch (Exception e) {
            result = null;
            error = e;
        }
        
        isDone.set(true);
        return error;
    }
}
