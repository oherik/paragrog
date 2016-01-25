import TSim.*;
import sun.management.Sensor;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

public class Lab1 {
    static Semaphore[] semaphores;
    public Lab1(Integer speed1, Integer speed2) {

        //Train trains = new Train();
        //Thread train1 = new Thread(trains, "1");
        //Thread train2 = new Thread(trains, "2");
        //Create and start the trains
        semaphores = new Semaphore[6];
        for (int i = 0; i< semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);   //Binary semaphore
        }
        Thread train1 = new Train(1,speed1, 16, 3, Direction.SOUTH);
        Thread train2 = new Train(2,speed2, 16, 11, Direction.NORTH);
        train1.start();
        train2.start();

    }

    public enum Direction {
        NORTH,SOUTH;
        public static Direction opposite(Direction direction){
           if(direction == NORTH){
               return SOUTH;
           }
            return NORTH;
        }
    }

    private class Train extends Thread{
        Integer id, speed;
        TSimInterface tsi;
        SensorEvent sensor;
        int latestY;
        Direction direction;

        private Train(Integer id, Integer speed, int x, int y, Direction direction) {
            this.id = id;
            this.speed = speed;
            this.direction = direction;
            latestY = y;
            tsi = TSimInterface.getInstance();

        }

        @Override
        public void run() {
            try {
                //Sätt tågen i rullning
                tsi.setSpeed(id, speed);

                while(true){
                   sensor = tsi.getSensor(id);
                    System.out.println(sensor);
                    if(sensor.getStatus() == SensorEvent.ACTIVE){         //Aktiv sensor
                        System.out.println("I status");
                        switch (sensor.getYpos()) {
                                case (11):case(13):
                                   if(direction == Direction.NORTH){
                                      //  System.out.println("I switch");
                                       while(!semaphores[4].tryAcquire()){
                                            tsi.setSpeed(id,0);
                                        }
                                       if(sensor.getYpos() == 11) {
                                           tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
                                       }else{
                                           tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
                                       }
                                        tsi.setSpeed(id, speed);
                                        semaphores[5].release();
                                        if(semaphores[3].tryAcquire()){
                                            tsi.setSwitch(4,9,tsi.SWITCH_LEFT);
                                        } else {
                                            tsi.setSwitch(4,9,tsi.SWITCH_RIGHT);
                                        }
                                    } else {
                                       semaphores[4].release();
                                       tsi.setSpeed(id, 0);
                                       wait(Math.min(1000, 1000 * Math.abs(speed)));
                                       tsi.setSpeed(id, -speed);
                                       direction = Direction.opposite(direction);

                                   }
                                    break;


                            }
                        }
                }

            } catch (CommandException e) {
                e.printStackTrace();    // or only e.getMessage() for the error
                System.exit(1);
            }
            catch (InterruptedException e){
                e.getStackTrace();
                System.err.println("Thread got interrupted for train " + id);
            }
        }

    }
}
