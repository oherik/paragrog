import TSim.*;

import java.awt.*;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

public class Lab1 {
    static Semaphore[] semaphores;
    public Lab1(Integer speed1, Integer speed2) {

        semaphores = new Semaphore[6];
        for (int i = 0; i< semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);
        }
        Thread train1 = new Train(1,speed1);
        Thread train2 = new Train(2,speed2);
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

        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();
            setupSensors();
        }

        @Override
        public void run() {
            try {
                direction = id == 1 ? Direction.SOUTH : Direction.NORTH;
                if(id == 2) semaphores[5].acquire();

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
                                semaphores[0].release();
                            }else{
                                stopForSemaphore(0);
                            }
                            break;
                        case 6:case 7:
                            if(direction == Direction.NORTH) {
                                semaphores[2].release();
                            }else{
                                stopForSemaphore(2);
                                if(index == 6) semaphores[1].release();
                                setSwitch(17, 7, index == 6);
                                setSwitch(15, 9, !semaphores[3].tryAcquire());
                            }
                            break;
                        case 8:case 9:
                            if(direction == Direction.NORTH) {
                                stopForSemaphore(2);
                                if(index == 8) semaphores[3].release();
                                setSwitch(15, 9 , index == 9);
                                setSwitch(17, 7, semaphores[1].tryAcquire());
                            }else{
                                semaphores[2].release();
                            }
                            break;
                        case 10:case 11:
                            if(direction == Direction.NORTH) {
                                semaphores[4].release();
                            }else{
                                stopForSemaphore(4);
                                if (index == 10) semaphores[3].release();
                                setSwitch(4, 9, index == 10);
                                setSwitch(3, 11, semaphores[5].tryAcquire());
                            }
                            break;
                        case 12:case 13:
                            if(direction == Direction.NORTH) {
                                stopForSemaphore(4);
                                if(index == 13) semaphores[5].release();
                                setSwitch(3, 11, index == 13);
                                setSwitch(4, 9, semaphores[3].tryAcquire());

                            }else{
                                semaphores[4].release();
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

        private void setSwitch(int x, int y, boolean left) throws CommandException {
            tsi.setSwitch(x, y, left ? tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);
        }

        private void stopForSemaphore(int semaphoreIndex) throws CommandException, InterruptedException {
            if(!semaphores[semaphoreIndex].tryAcquire()) {
                tsi.setSpeed(id, 0);
                semaphores[semaphoreIndex].acquire();
                tsi.setSpeed(id, speed);
            }
        }

        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(Math.min(1000, 1000 * Math.abs(speed)));
            speed = -speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id,speed);
        }

        private void setupSensors() {
            sensors = new ArrayList(16);
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
            sensors.add(new Point(15,13));
            sensors.add(new Point(15,11));
        }
    }
}