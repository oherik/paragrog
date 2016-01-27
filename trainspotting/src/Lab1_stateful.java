import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.concurrent.Semaphore;

public class Lab1_stateful {
    static Semaphore[] semaphores;

    public Lab1_stateful(Integer speed1, Integer speed2) {

        semaphores = new Semaphore[6];
        for (int i = 0; i < semaphores.length; i++) {
            semaphores[i] = new Semaphore(1);   //Binary semaphore
        }
        Thread train1 = new Train(1, speed1);
        Thread train2 = new Train(2, speed2);
        train1.start();
        train2.start();
    }

    public enum Direction {
        NORTH, SOUTH;
        public static Direction opposite(Direction direction) {
            return direction == NORTH ? SOUTH : NORTH;
        }
    }

    private class Train extends Thread {
        Integer id, speed;
        TSimInterface tsi;
        Direction direction;
        int currentSection;
        boolean firstSensor;
        boolean onDefaultTrack;

        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();
        }

        @Override
        public void run() {
            try {
                tsi.setDebug(false);
                tsi.setSpeed(id, speed);
                if (id == 1) {
                    direction = Direction.SOUTH;
                    currentSection = -1;
                    onDefaultTrack = false;
                } else {
                    direction = Direction.NORTH;
                    currentSection = 5;
                    onDefaultTrack = true;
                    semaphores[5].acquire();
                }

                while (true) {
                    if (tsi.getSensor(id).getStatus() == SensorEvent.ACTIVE) {         //Active sensor
                        switch (currentSection) {
                            case -1:
                                if (direction == Direction.NORTH) {
                                    if (firstSensor) {
                                        semaphores[currentSection + 1].release();
                                        firstSensor = !firstSensor;
                                    } else {
                                        atStation();
                                    }
                                } else {
                                    if (!firstSensor) {
                                        waitForNextSection();
                                        firstSensor = !firstSensor;
                                        currentSection += 2;
                                    }
                                }
                                break;
                            case 1:
                                if (firstSensor) {
                                    releaseLastSection();
                                } else {
                                    waitForNextSection();
                                    if (direction == Direction.SOUTH) {
                                        if (onDefaultTrack) semaphores[currentSection].release();
                                        setSwitch(17, 7, onDefaultTrack);
                                        onDefaultTrack = semaphores[currentSection + 2].tryAcquire();
                                        setSwitch(15, 9, !onDefaultTrack);
                                    }
                                    currentSection = direction == Direction.NORTH ? currentSection - 2 : currentSection + 2;
                                }
                                firstSensor = !firstSensor;
                                break;
                            case 3:
                                if (firstSensor) {
                                    releaseLastSection();
                                } else {
                                    waitForNextSection();
                                    if (direction == Direction.NORTH) {
                                        if (onDefaultTrack) semaphores[currentSection].release();
                                        setSwitch(15, 9, !onDefaultTrack);
                                        onDefaultTrack = semaphores[currentSection - 2].tryAcquire();
                                        setSwitch(17, 7, onDefaultTrack);
                                    } else {
                                        if (onDefaultTrack) semaphores[currentSection].release();
                                        setSwitch(4, 9, onDefaultTrack);
                                        onDefaultTrack = semaphores[currentSection + 2].tryAcquire();
                                        setSwitch(3, 11, onDefaultTrack);
                                    }
                                    currentSection = direction == Direction.NORTH ? currentSection - 2 : currentSection + 2;
                                }

                                firstSensor = !firstSensor;
                                break;
                            case 5:
                                if (direction == Direction.NORTH) {
                                    if (onDefaultTrack) semaphores[currentSection].release();
                                    if (!firstSensor) {
                                        waitForNextSection();
                                        setSwitch(3, 11, onDefaultTrack);
                                        onDefaultTrack = semaphores[currentSection - 2].tryAcquire();
                                        setSwitch(4, 9, onDefaultTrack);
                                        currentSection -= 2;
                                    }
                                    firstSensor = !firstSensor;
                                } else if (direction == Direction.SOUTH) {
                                    if (firstSensor) {
                                        semaphores[currentSection - 1].release();
                                        firstSensor = !firstSensor;
                                    } else {
                                        atStation();
                                    }
                                }
                                break;
                        }

                    }   //End sensor active
                }   //End while(true)

            } catch (CommandException e) {
                e.printStackTrace();    // or only e.getMessage() for the error
                System.exit(1);
            } catch (InterruptedException e) {
                e.getStackTrace();
                System.err.println("Thread got interrupted for train " + id);
            }
        }

        private void setSwitch(int x, int y, boolean left) throws CommandException {
            tsi.setSwitch(x, y, left ? tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);
        }

        private void releaseLastSection() {
            semaphores[direction == Direction.NORTH ? currentSection + 1 : currentSection - 1].release();
        }

        private void waitForNextSection() throws CommandException, InterruptedException {
            tsi.setSpeed(id, 0);
            semaphores[direction == Direction.NORTH ? currentSection - 1 : currentSection + 1].acquire();            //TODO fråga om detta
            tsi.setSpeed(id, speed);
        }

        private void atStation() throws CommandException, InterruptedException {
            tsi.setSpeed(id, 0);
            sleep(Math.min(1000, 1000 * Math.abs(speed)));
            speed = -speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id, speed);
        }
    }
}