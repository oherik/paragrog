import TSim.*;

import java.awt.*;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

public class Lab1 {
    static Semaphore[] semaphores;
    public Lab1(Integer speed1, Integer speed2) {

        semaphores = new Semaphore[6];
        for (int i = 0; i< semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);   //Binary semaphore
        }
        Thread train1 = new Train(1,speed1);
        Thread train2 = new Train(2,speed2);
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

        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();

        }

        @Override
        public void run() {
            try {
                tsi.setSpeed(id, speed);
                //Start clauses
                //TODO Hårdkodat, ändra sen kanske
                if(id == 1){
                    latestY = 3;
                    direction = Direction.SOUTH;
                } else {
                    latestY = 11;
                    direction = Direction.NORTH;
                    semaphores[5].acquire();
                }

                while(true){
                   sensor = tsi.getSensor(id);
                    System.out.println(sensor);
                    if(sensor.getStatus() == SensorEvent.ACTIVE){         //Active sensor

                        /*
                        switch (sensor.getYpos()) {
                            case(3): case(5):
                                if(direction == Direction.NORTH){
                                    semaphores[0].release();
                                    tsi.setSpeed(id, 0);
                                    sleep(Math.min(1000, 1000 * Math.abs(speed)));
                                    direction = Direction.opposite(direction);
                                    speed = -speed;
                                    tsi.setSpeed(id, speed);
                                } else {
                                    while(!semaphores[0].tryAcquire()){
                                        tsi.setSpeed(id,0);
                                    }
                                    tsi.setSpeed(id, speed);
                                }
                                break;


                            case(8): case(7):                          // The second upper two signal
                                if(direction == Direction.NORTH) {
                                    semaphores[2].release();
                                    while (!semaphores[0].tryAcquire()) {
                                        tsi.setSpeed(id, 0);
                                    }
                                }else {
                                    semaphores[0].release();
                                    while (!semaphores[2].tryAcquire()) {
                                        semaphores[3].tryAcquire();
                                        tsi.setSpeed(id, 0);
                                    }
                                    if(sensor.getYpos() == 8) {
                                        semaphores[1].release();
                                        tsi.setSwitch(17, 7, tsi.SWITCH_LEFT);
                                    }else{
                                        tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
                                    }
                                    if(semaphores[3].tryAcquire()){
                                        System.out.println("acq");
                                        tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
                                    } else {
                                        System.out.println("upptagen");
                                        tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
                                    }

                                }
                                tsi.setSpeed(id, speed);


                                break;


                            case(10): case(9):                          // The second lowest two signal
                                if(direction == Direction.NORTH){
                                    semaphores[4].release();
                                    while(!semaphores[2].tryAcquire()){
                                        tsi.setSpeed(id,0);
                                    }
                                    if(sensor.getYpos() == 10) {
                                        tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
                                    }else{
                                        tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
                                    }

                                    if(semaphores[1].tryAcquire()){
                                        tsi.setSwitch(17,7,tsi.SWITCH_LEFT);
                                    } else {
                                        tsi.setSwitch(17,7,tsi.SWITCH_RIGHT);
                                    }

                                } else {
                                    semaphores[2].release();
                                    while(!semaphores[4].tryAcquire()){
                                        tsi.setSpeed(id,0);
                                    }
                                    if(sensor.getYpos() == 10) {
                                        tsi.setSwitch(4, 9, tsi.SWITCH_RIGHT);
                                    }else{
                                        tsi.setSwitch(4, 9, tsi.SWITCH_LEFT);
                                    }
                                    if(semaphores[5].tryAcquire()){
                                        tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
                                    } else {
                                        tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
                                    }
                                }

                                if(sensor.getYpos()==9) {
                                    System.out.println("releasad");
                                    semaphores[3].release();
                                }
                                tsi.setSpeed(id, speed);
                                break;



                            case(11): case(13):                       // The lower two signals
                                if(direction == Direction.SOUTH){
                                    semaphores[4].release();

                                }

                                while(!semaphores[4].tryAcquire()){
                                    tsi.setSpeed(id,0);
                                }
                                if(sensor.getYpos() == 11) {
                                    semaphores[4].release();
                                    tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
                                }else{
                                    tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
                                }

                                if(semaphores[3].tryAcquire()){
                                    tsi.setSwitch(4,9,tsi.SWITCH_LEFT);
                                } else {
                                    tsi.setSwitch(4,9,tsi.SWITCH_RIGHT);
                                }
                                tsi.setSpeed(id, speed);
                                break;




                            }   // End switch
                            */
                        }   //End sensor active
                }   //End while(true)

            } catch (CommandException e) {
                e.printStackTrace();    // or only e.getMessage() for the error
                System.exit(1);
            }
            catch (InterruptedException e){
                e.getStackTrace();
                System.err.println("Thread got interrupted for train " + id);
            }

        }


        /*
        Erik's test methods
         */
        /*

                Point sensorCoordinates;
        ArrayList sensors;
        int currentSensor;


                    sensorCoordinates = new Point();
            sensors = new ArrayList<Point>(16);
            sensors.add(new Point(15,4));
            sensors.add(new Point(15,6));
            sensors.add(new Point(7,7));
            sensors.add(new Point(8,6));
            sensors.add(new Point(9,8));
            sensors.add(new Point(9,7));
            sensors.add(new Point(16,8));
            sensors.add(new Point(16,7));
            sensors.add(new Point(14,9));
            sensors.add(new Point(14,11));
            sensors.add(new Point(5,9));
            sensors.add(new Point(5,10));
            sensors.add(new Point(4,13));
            sensors.add(new Point(4,11));
            sensors.add(new Point(15,13));
            sensors.add(new Point(15,11));

            i while-loopen
              sensorCoordinates.setLocation(sensor.getXpos(), sensor.getYpos());
              currentSensor = sensors.indexOf(sensorCoordinates);
              System.out.println(currentSensor);


         +   +    0, 1
         |   |              (Section -1)
         |   |
         +   +    2, 3
          X X
           X               Section 0
          X X
         +   +    4, 5
         |   |             Section 1
         |   |
         +   +    6, 7
          X X
           X               Section 2
          X X
         +   +    8, 9
         |   |             Section 3
         |   |
         +   +    10, 11
          X X
           X               Section 4
          X X
         +   +    12, 13
         |   |             Section 5
         |   |
         +   +    14, 15

        Lokala variabler:
            Riktning
            nuvarande sektion
            hastighet
            ID

         Om NORR
            Om sensor 0, 1
                atStation();
            Annars om sensor 2, 3, 6, 7, 10, 11
                släppNuvarande()
                nuvarande sektion = nuvarande sektion - 1
            Annars
                taNästa()
                Om sensor 4, 8, 13          //Defaultväg
                       släppNuvarande();
                       Sätt relevant switch
                Annars
                        Sätt relevant switch
                OM INTE 4, 5
                    OM testaNästNästa()
                         Sätt relevant switch
                    Annars
                        Sätt relevant switch

         Om SÖDER
            Om sensor 14, 15
                atStation();
            Annars om sensor 4, 5, 8, 9, 12, 13
                släppNuvarande()
                nuvarande sektion = nuvarande sektion + 1
            Annars
                taNästa()
                Om sensor 6, 10          //Defaultväg
                       släppNuvarande();
                       Sätt relevant switch
                Annars
                        Sätt relevant switch
                    OM testaNästNästa()
                         Sätt relevant switch
                    Annars
                        Sätt relevant switch



        Metoder:

        atStation(){
                Stanna
                Vänta
                Byt håll
        }

        testaNästNästa(){
            index = NORR ? nuvarande sektion -2 : nuvarande sektion + 2;
            return semafor[index].tryAcquire();
        }

        taNästa(){
            index = NORR ? nuvarande sektion -1 : nuvarande sektion + 1;
            semafor[index].acquire();
                vänta
        }
        släppNuvarande(){
            semafor[nuvarande sektion].release();
        }




         */

        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(Math.min(1000, 1000 * Math.abs(speed)));
            speed = - speed;
            direction = Direction.opposite(direction);
        }

    }
}
