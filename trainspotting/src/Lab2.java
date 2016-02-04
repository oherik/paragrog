import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.awt.*;
import java.util.ArrayList;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab2 {
    static TrackMonitor[] monitors;
    public Lab2(Integer speed1, Integer speed2) {

        monitors = new TrackMonitor[6];
        for (int i = 0; i< monitors.length; i++) {
            monitors[i] = new TrackMonitor();
        }
        Train train1 = new Train(1,speed1);
        Train train2 = new Train(2,speed2);
        train1.start();
        train2.start();
    }

    /**
     * Class for the direction of the train. The direction can be either North or South.
     * Method opposite changes the direction to the opposite direction.
     */
    public enum Direction {
        NORTH,SOUTH;
        public static Direction opposite(Direction direction) {
            return direction == NORTH ? SOUTH : NORTH;
        }
    }

    private class TrackMonitor  {
        private final Lock lock = new ReentrantLock();
        private Condition trackEmpty = lock.newCondition();
        private boolean trainOnTrack = false;

        public void enter() {
            lock.lock();
            try {
                while(trainOnTrack) {
                    trackEmpty.await();
                }
                trainOnTrack = true;
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                lock.unlock();
            }
        }

        public void leave() {
            lock.lock();
            try {
                trainOnTrack = false;
                trackEmpty.signal();
            }finally {
                lock.unlock();
            }
        }

        public boolean tryEnter() {
            if(trainOnTrack) {
                return false;
            }else{
                enter();
                return true;
            }
        }

    }


    /**
     * Class for trains and how they drive on the track.
     */
    private class Train extends Thread{
        Integer id, speed;
        TSimInterface tsi;
        SensorEvent sensor;
        Direction direction;
        ArrayList sensors;
        boolean[] holdingMonitor;
        private Train(Integer id, Integer speed) {
            this.id = id;
            this.speed = speed;
            tsi = TSimInterface.getInstance();
            setupSensors();
            holdingMonitor = new boolean[7];
        }

        @Override
        public void run() {
            try {
                //Start conditions depending on train ID.
                direction = id == 1 ? Direction.SOUTH : Direction.NORTH;
                if(id == 2) {
                    monitors[5].enter();
                    holdingMonitor[5] = true;
                }
                tsi.setSpeed(id, speed);

                //loop to keep train going
                while(true){
                    //Check which sensor the train passes
                   sensor = tsi.getSensor(id);
                    if(sensor.getStatus() == SensorEvent.ACTIVE){
                        int index = sensors.indexOf(new Point(sensor.getXpos(), sensor.getYpos()));
                        switch(index){
                        //The train is at a station and should change direction.
                        case 0:case 1:case 14:case 15:
                            if((direction == Direction.NORTH && (index == 0 || index == 1)) ||
                                        (direction == Direction.SOUTH && (index == 14 || index == 15))) {
                                changeDirection();
                            }
                            break;
                        //The train is passing/has passed the crossing and aquires/releases the monitor for that crossing.
                        case 2:case 3:case 4:case 5:
                            if((direction == Direction.NORTH && (index == 2 || index == 3))
                                        || (direction == Direction.SOUTH && (index == 4 || index == 5))) {
                                release(0);
                            }else   stopForMonitor(0);
                            break;
                        //The train is leaving/going onto the first single track.
                        case 6:case 7:
                            if(direction == Direction.NORTH)
                                release(2);
                            else{
                                stopForMonitor(2);
                                if(holdingMonitor[1])
                                    release(1);
                                setSwitch(17, 7, index == 6);
                            }
                            break;
                            //The northern two sensors on section 3 (middle double track)
                        case 8:case 9:
                            if(direction == Direction.NORTH) {
                                stopForMonitor(2);
                                setSwitch(15, 9, index == 9);
                            }else
                                release(2);
                            break;
                            //The southern two sensors on section 3 (middle double track)
                        case 10:case 11:
                            if(direction == Direction.NORTH)
                                release(4);
                            else{
                                stopForMonitor(4);
                                setSwitch(4, 9, index == 10);
                            }
                            break;
                            //The northern two sensors on section 5 (southern double track)
                        case 12:case 13:
                            if(direction == Direction.NORTH) {
                                stopForMonitor(4);
                                if(holdingMonitor[5])
                                    release(5);
                                setSwitch(3, 11, index == 13);
                            }else{
                                release(4);
                            }
                            break;
                             /*  The sensor north of section 3 (middle double track)
                            It will here be decided if the train will take the default or the secondary track, if going
                            south. If it's on its way north it will release the default track monitor for section 3.
                            as well as try to acquire the default monitor for the next double track section(i.e. 1).
                             */
                            case 16:
                                if(direction == Direction.SOUTH) {
                                    setSwitch(15, 9, !(holdingMonitor[3] = monitors[3].tryEnter()));
                                }   else {
                                    if (holdingMonitor[3])
                                        release(3);
                                    setSwitch(17, 7, holdingMonitor[1] = monitors[1].tryEnter());
                                }
                                break;
                            /*  The sensor south of section 3 (middle double track)
                            It will here be decided if the train will take the default or the secondary track, if going
                            north. If it's on its way south it will release the default track monitor for section 3,
                            as well as try to acquire the default monitor for the next double track section(i.e. 5).
                           */
                            case 17:
                                if(direction == Direction.NORTH){
                                    setSwitch(4, 9, holdingMonitor[3] = monitors[3].tryEnter());
                                } else{
                                    if (holdingMonitor[3])
                                        release(3);
                                    setSwitch(3, 11, holdingMonitor[5] = monitors[5].tryEnter());
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

        /**
         * Releases a monitor as well as making sure the train remembers that it doesn't hold it anymore
         * @param index The index of the monitor to release
         */
        private void release(int index){
            monitors[index].leave();
            holdingMonitor[index] = false;
        }

        /**
         * Sets a switch either left or right
         * @param x x coordinate
         * @param y y coordinate
         * @param left  Set to true if the switch is to be set to LEFT, otherwise the swich will be set to RIGHT
         * @throws CommandException
         */
        private void setSwitch(int x, int y, boolean left) throws CommandException {
            tsi.setSwitch(x, y, left ? tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT);
        }

        /**
         *  Tries to acquire the monitor for the next single track. If the train doesn't get it it slows down and then
         *  tries again when it has passed the sensors. If it doesn't get the monitor it stops and waits for it to
         *  become available. When it finally gets the monitor the train starts going at its original speed.
         * @param index    The monitor to acquire
         * @throws CommandException
         * @throws InterruptedException
         */
        private void stopForMonitor(int index) throws CommandException, InterruptedException {
            if(!monitors[index].tryEnter()){
                int slow = (int) (Math.min(Math.abs(speed),5) * Math.signum(speed));
                tsi.setSpeed(id, slow);
                boolean hasPassedSensor = tsi.getSensor(id).getStatus() == SensorEvent.INACTIVE;
                if(hasPassedSensor && !monitors[index].tryEnter()){
                    tsi.setSpeed(id, 0);
                    monitors[index].enter();
                }
                tsi.setSpeed(id, speed);
            }
            holdingMonitor[index] = true;
        }

        /**
         * Stops the train, waits a few seconds and then reverse the direction (as well as the speed)
         * @throws CommandException
         * @throws InterruptedException
         */
        private void changeDirection() throws CommandException, InterruptedException{
            tsi.setSpeed(id, 0);
            sleep(1000 + (20 * Math.abs(speed)));
            speed = -speed;
            direction = Direction.opposite(direction);
            tsi.setSpeed(id, speed);
        }

        /**
         * Initialize all the sensors. Uses the coordinates of the sensors and store them in a list for easy access.
         */
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