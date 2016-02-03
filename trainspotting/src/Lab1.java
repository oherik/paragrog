import TSim.*;

import java.awt.*;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.Semaphore;

public class Lab1 {
    static Semaphore[] semaphores;
    public Lab1(Integer speed1, Integer speed2) {

        semaphores = new Semaphore[6];
        for (int i = 0; i< semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);
        }
        Train train1 = new Train(1,22);
        Train train2 = new Train(2,22);
        train1.start();
        train2.start();
    }

    public enum Direction {
        NORTH,SOUTH;
        public static Direction opposite(Direction direction) {
            return direction == NORTH ? SOUTH : NORTH;
        }
    }

    private class Train extends Thread{
        Integer id, speed;
        TSimInterface tsi;
        SensorEvent sensor;
        Direction direction;
        ArrayList sensors;
        boolean[] holdingSemaphore;
        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();
            setupSensors();
            holdingSemaphore = new boolean[7];
        }

        @Override
        public void run() {
            try {
               // tsi.setDebug(false);
                direction = id == 1 ? Direction.SOUTH : Direction.NORTH;
                if(id == 2) {
                    semaphores[5].acquire();
                    holdingSemaphore[5] = true;
                }
                tsi.setSpeed(id, speed);

                while(true){
                   sensor = tsi.getSensor(id);
                    if(sensor.getStatus() == SensorEvent.ACTIVE){
                        int index = sensors.indexOf(new Point(sensor.getXpos(), sensor.getYpos()));
                        switch(index){
                        case 0:case 1:case 14:case 15:
                            if((direction == Direction.NORTH && (index == 0 || index == 1)) ||
                                        (direction == Direction.SOUTH && (index == 14 || index == 15))) {
                                changeDirection();
                            }
                            break;
                        case 2:case 3:case 4:case 5:
                            if((direction == Direction.NORTH && (index == 2 || index == 3))
                                        || (direction == Direction.SOUTH && (index == 4 || index == 5))) {
                                release(0);
                            }else   stopForSemaphore(0);
                            break;
                        case 6:case 7:
                            if(direction == Direction.NORTH)
                                release(2);
                            else{
                                stopForSemaphore(2);
                                if(holdingSemaphore[1])
                                    release(1);
                                setSwitch(17, 7, index == 6);
                            }
                            break;
                        case 8:case 9:
                            if(direction == Direction.NORTH) {
                                stopForSemaphore(2);
                                setSwitch(15, 9, index == 9);
                            }else
                                release(2);
                            break;
                        case 10:case 11:
                            if(direction == Direction.NORTH)
                                release(4);
                            else{
                                stopForSemaphore(4);
                                setSwitch(4, 9, index == 10);
                            }
                            break;
                        case 12:case 13:
                            if(direction == Direction.NORTH) {
                                stopForSemaphore(4);
                                if(holdingSemaphore[5])
                                    release(5);
                                setSwitch(3, 11, index == 13);
                            }else{
                                release(4);
                            }
                            break;
                           case 16:
                                if(direction == Direction.SOUTH) {
                                    setSwitch(15, 9, !(holdingSemaphore[3] = semaphores[3].tryAcquire()));
                                }   else {
                                    if (holdingSemaphore[3])
                                        release(3);
                                    setSwitch(17, 7, holdingSemaphore[1] = semaphores[1].tryAcquire());
                                }
                                break;
                            case 17:
                                if(direction == Direction.NORTH){
                                    setSwitch(4, 9, holdingSemaphore[3] = semaphores[3].tryAcquire());
                                } else{
                                    if (holdingSemaphore[3])
                                        release(3);
                                    setSwitch(3, 11, holdingSemaphore[5] = semaphores[5].tryAcquire());
                                }
                                break;
                        }
                    }   //End sensor active
                }   //End while(true)

            } catch (CommandException e) {
                e.printStackTrace();
                System.exit(1);
            }
            catch (InterruptedException e){
                e.getStackTrace();
                System.exit(1);
            }
        }

        private void release(int index){
            semaphores[index].release();
            holdingSemaphore[index] = false;
        }

        private void setSwitch(int x, int y, boolean left) throws CommandException {
            tsi.setSwitch(x, y, left ? tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);
        }

        private void stopForSemaphore(int semaphoreIndex) throws CommandException, InterruptedException {
            if(!semaphores[semaphoreIndex].tryAcquire()){
                int slow = (int) (Math.min(Math.abs(speed),5) * Math.signum(speed));
                tsi.setSpeed(id, slow);
                boolean hasPassedSensor = tsi.getSensor(id).getStatus() == SensorEvent.INACTIVE;
                if(hasPassedSensor && !semaphores[semaphoreIndex].tryAcquire()){
                    tsi.setSpeed(id, 0);
                    semaphores[semaphoreIndex].acquire();
                }
                tsi.setSpeed(id, speed);
            }
            holdingSemaphore[semaphoreIndex] = true;
        }

        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(1000 + (20 * Math.abs(speed)));
            speed = -speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id, speed);
            if(id == 1){
                Random r = new Random();
               speed = (int)Math.signum(speed)*(r.nextInt((22 - 1) + 1) + 1);
            }
        }

        private void setupSensors() {
            sensors = new ArrayList(16);
            sensors.add(new Point(15,3));
            sensors.add(new Point(15,5));
            sensors.add(new Point(6,6));
            sensors.add(new Point(9,5));
            sensors.add(new Point(10,8));
            sensors.add(new Point(11,7));
            sensors.add(new Point(15,8));
            sensors.add(new Point(14,7));
            sensors.add(new Point(12,9));
            sensors.add(new Point(13,10));
            sensors.add(new Point(7,9));
            sensors.add(new Point(6,10));
            sensors.add(new Point(4,13));
            sensors.add(new Point(6,11));
            sensors.add(new Point(15,13));
            sensors.add(new Point(15,11));
            sensors.add(new Point(16,9));
            sensors.add(new Point(3,9));
        }
    }
}