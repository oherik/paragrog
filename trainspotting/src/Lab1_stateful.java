import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.awt.*;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

public class Lab1_stateful {
    static Semaphore[] semaphores;
    public Lab1_stateful(Integer speed1, Integer speed2) {

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
        Direction direction;
        int currentSection;
        Point sensorCoordinates;
        ArrayList sensors;
        boolean firstSensor;
        boolean onDefaultTrack;

        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();

            sensorCoordinates = new Point();
            sensors = new ArrayList<Point>(16);
            sensors.add(new Point(15,3));
            sensors.add(new Point(15,5));
            sensors.add(new Point(6,7));
            sensors.add(new Point(8,5));
            sensors.add(new Point(10,8));
            sensors.add(new Point(10,7));
            sensors.add(new Point(15,8));
            sensors.add(new Point(15,7));
            sensors.add(new Point(12,9));
            sensors.add(new Point(13,10));
            sensors.add(new Point(7,9));
            sensors.add(new Point(6,10));
            sensors.add(new Point(4,13));
            sensors.add(new Point(5,11));
            sensors.add(new Point(15, 13));
            sensors.add(new Point(15,11));

        }

        @Override
        public void run() {
            try {
               // tsi.setDebug(false);
                tsi.setSpeed(id, speed);
                //Start clauses
                //TODO Hårdkodat, ändra sen kanske
                if(id == 1){
                    direction = Direction.SOUTH;
                    currentSection = -1;
                    onDefaultTrack = false;
                }
                else{
                    direction = Direction.NORTH;
                    currentSection = 5;
                    onDefaultTrack = true;
                    semaphores[5].acquire();
                }

                while(true){
                   sensor = tsi.getSensor(id);




                    if(sensor.getStatus() == SensorEvent.ACTIVE){         //Active sensor
                        switch(currentSection){
                            case -1:
                                if(direction == Direction.NORTH){
                                    if(firstSensor) {
                                       semaphores[currentSection-1].release();
                                       firstSensor = !firstSensor;
                                   }else {
                                       changeDirection();
                                   }
                               } else {
                                    if(!firstSensor){
                                        acquireNextSemaphore();
                                        firstSensor = !firstSensor;
                                    }
                                }
                                break;
                            case 1:
                                if(direction == Direction.NORTH){
                                    if(firstSensor){
                                        currentSection = currentSection-2;
                                        releaseLastSemaphore();
                                    } else if(onDefaultTrack){
                                            releaseCurrentSemaphore();
                                        }
                                } else {
                                    if (firstSensor) {
                                        currentSection += 2;
                                        System.out.println("updated to " + currentSection);
                                        releaseLastSemaphore();
                                    } else {
                                        acquireNextSemaphore();
                                        if (onDefaultTrack) {
                                            releaseCurrentSemaphore();
                                            tsi.setSwitch(17, 7, tsi.SWITCH_LEFT);
                                        } else {
                                            tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
                                        }
                                        if (testNextNextSemaphore()) {
                                            onDefaultTrack = true;
                                            tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
                                        } else {
                                            onDefaultTrack = false;
                                            tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
                                        }
                                    }
                                }
                                firstSensor= !firstSensor;
                                break;
                            case 3:

                                firstSensor= !firstSensor;
                                break;
                            case 5:
                                break;





                        }





                        sensorCoordinates.setLocation(sensor.getXpos(), sensor.getYpos());
                        int index = sensors.indexOf(sensorCoordinates) + 1;
                        if(direction == Direction.NORTH){
                            switch (index) {
                                case 15:case 16:
                                    break;
                                case 1:case 2:
                                    changeDirection();
                                    break;
                                case 3:case 4: case 7: case 8: case 11:case 12:
                                    currentSection = currentSection-2;
                                    releaseLastSemaphore();

                                    break;
                                default:
                                    acquireNextSemaphore();
                                    if(index == 5){    //Defaultväg
                                        releaseCurrentSemaphore();
                                    } else if(index!=6){
                                            if(index == 9 || index == 10) {
                                                if(index == 9){
                                                    releaseCurrentSemaphore();
                                                    tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
                                                } else {
                                                    tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
                                                }

                                                if (testNextNextSemaphore()) {
                                                    tsi.setSwitch(17, 7, tsi.SWITCH_LEFT);
                                                } else {
                                                    tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
                                                }
                                            } else {
                                                if(index == 13) {
                                                    tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
                                                } else {
                                                    releaseCurrentSemaphore();
                                                    tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
                                                }
                                                if (testNextNextSemaphore()) {
                                                    tsi.setSwitch(4,9,tsi.SWITCH_LEFT);
                                                } else {
                                                    tsi.setSwitch(4,9,tsi.SWITCH_RIGHT);
                                                }
                                            }
                                    }
                                    break;
                            }
                        }
                            else{
                                switch (index) {
                                    case 1:case 2:
                                        break;
                                    case 15: case 16:
                                        changeDirection();
                                        break;
                                    case 9:  case 10: case 13:case 14:case 5:case 6:
                                        currentSection+=2;
                                        System.out.println("updated to " + currentSection);
                                        releaseLastSemaphore();

                                        break;
                                    default:
                                        acquireNextSemaphore();
                                        if (index != 3 && index != 4) {
                                            if (index == 7 || index == 8) {
                                                if (index == 7) {
                                                    releaseCurrentSemaphore();
                                                    tsi.setSwitch(17, 7, tsi.SWITCH_LEFT);
                                                } else {
                                                    tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
                                                }
                                                if (testNextNextSemaphore()) {
                                                    tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
                                                } else {
                                                    tsi.setSwitch(15, 9, tsi.SWITCH_LEFT);
                                                }
                                            } else {
                                                if (index == 11) {
                                                    releaseCurrentSemaphore();
                                                    tsi.setSwitch(4, 9, tsi.SWITCH_LEFT);
                                                } else {
                                                    tsi.setSwitch(4, 9, tsi.SWITCH_RIGHT);
                                                }
                                                if (testNextNextSemaphore()) {
                                                    tsi.setSwitch(3, 11, tsi.SWITCH_LEFT);
                                                } else {
                                                    tsi.setSwitch(3, 11, tsi.SWITCH_RIGHT);
                                                }
                                            }

                                        }
                                        break;


                                }
                                }



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

        private void releaseCurrentSemaphore(){
            semaphores[currentSection].release();
            System.out.println(id + " released " + currentSection);
        }
        private void releaseLastSemaphore(){
            int index = (direction == Direction.NORTH ? currentSection + 1 : currentSection - 1);
            semaphores[index].release();
            System.out.println(id + " released " + index);
        }

        private boolean testNextNextSemaphore(){
            int index = direction == Direction.NORTH ? currentSection - 2 : currentSection + 2;
            if(semaphores[index].tryAcquire()){
                System.out.println(id + " acquired next next  " + index);
                return true;
            }
            else
                System.out.println(id + " didn't acquired next next  " + index);
            return false;
        }

        private void acquireNextSemaphore() throws CommandException{
            int index = direction == Direction.NORTH ? currentSection - 1 : currentSection + 1;
            while (!semaphores[index].tryAcquire()) {
                tsi.setSpeed(id, 0);
            }
            System.out.println(id + " acquired next " + index);
            tsi.setSpeed(id,speed);
        }

        private int getIndexOfSensor(int xpos, int ypos) {
            switch (ypos) {
                case (5):
                    switch (xpos) {
                        case (15):
                            return 1;
                        case (8):
                            return 3;
                        default:
                            return 0;
                    }
                case (3):
                    return 2;
                case (7):
                    switch (xpos) {
                        case (6):
                            return 4;
                        case (10):
                            return 5;
                        case (15):
                            return 6;
                        default:
                            return 0;
                    }
                case (8):
                    switch (xpos) {
                        case (10):
                            return 7;
                        case (15):
                            return 8;
                        default:
                            return 0;
                    }
                case (9):
                    switch (xpos) {
                        case (12):
                            return 9;
                        case (7):
                            return 10;
                        default:
                            return 0;
                    }
                case (10):
                    switch (xpos) {
                        case (13):
                            return 11;
                        case (6):
                            return 13;
                        default:
                            return 0;
                    }
                case (11):
                    switch (xpos) {
                        case (5):
                            return 13;
                        case (15):
                            return 14;
                        default:
                            return 0;
                    }
                case (13):
                    switch (xpos) {
                        case (4):
                            return 15;
                        case (15):
                            return 16;
                        default:
                            return 0;
                    }
            }
            return 0;

        }
        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(Math.min(1000, 1000 * Math.abs(speed)));
            speed = - speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id,speed);
        }

    }
}
