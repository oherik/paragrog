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
                //Start clauses
                if(id == 1){
                    direction = Direction.SOUTH;
                }else{
                    direction = Direction.NORTH;
                    semaphores[5].acquire();
                }
                tsi.setSpeed(id, speed);

                while(true){
                   sensor = tsi.getSensor(id);
                    if(sensor.getStatus() == SensorEvent.ACTIVE){ //Active sensor

                        int index = sensors.indexOf(new Point(sensor.getXpos(), sensor.getYpos()));
                        switch(index){
                        case 1:case 2:case 15:case 16:
                            if((direction == Direction.NORTH && (index == 1 || index == 3)) ||
                                        (direction == Direction.SOUTH && (index == 15 || index == 16))) {
                                changeDirection();
                            }
                            break;
                        case 3:case 4:case 5:case 6:
                            if((direction == Direction.NORTH && (index == 3 || index == 4))
                                        || (direction == Direction.SOUTH && (index == 5 || index == 6))) {
                                semaphores[0].release();
                            }else{
                                driveThroughCriticalSection(0);
                            }
                            break;
                        case 7:case 8:
                            if(direction == Direction.NORTH) {
                                semaphores[2].release();
                            }else{
                                driveThroughCriticalSection(2);
                                if(sensor.getYpos() == 8) semaphores[1].release();
                                setSwitch(17, 7, sensor.getYpos() == 8);
                                setSwitch(15, 9, !semaphores[3].tryAcquire());
                            }
                            break;
                        case 9:case 10:
                            if(direction == Direction.NORTH) {
                                driveThroughCriticalSection(2);
                                if(sensor.getYpos() == 10) semaphores[3].release();
                                setSwitch(15, 9 , sensor.getYpos() ==10);
                                setSwitch(17, 7, semaphores[1].tryAcquire());
                            }else{
                                semaphores[2].release();
                            }
                            break;
                        case 11:case 12:
                            if(direction == Direction.NORTH) {
                                semaphores[4].release();
                            }else{
                                driveThroughCriticalSection(4);
                                if (sensor.getYpos() == 9) semaphores[3].release();
                                setSwitch(4, 9, sensor.getYpos() == 9);
                                setSwitch(3, 11, semaphores[5].tryAcquire());
                            }
                            break;
                        case 13:case 14:
                            if(direction == Direction.NORTH) {
                                driveThroughCriticalSection(4);
                                if(sensor.getYpos() == 11) semaphores[5].release();
                                setSwitch(3, 11, sensor.getYpos() == 11);
                                setSwitch(4, 9, semaphores[3].tryAcquire());

                            }else{
                                semaphores[4].release();
                            }
                            break;
                        }
                    }   //End sensor active
                }   //End while(true)

            } catch (CommandException e) {
                e.printStackTrace();    // or only e.getMessage() for the error
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

        private void driveThroughCriticalSection(int semaphoreIndex) throws CommandException {
            while(!semaphores[semaphoreIndex].tryAcquire()) {
                tsi.setSpeed(id, 0);
            }
            tsi.setSpeed(id, speed);
        }

        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(Math.min(1000, 1000 * Math.abs(speed)));
            speed = - speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id,speed);
        }

        private void setupSensors() {
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
            sensors.add(new Point(15,13));
            sensors.add(new Point(15,11));
        }
    }
}