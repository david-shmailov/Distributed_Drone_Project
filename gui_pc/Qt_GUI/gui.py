import argparse
import os.path
import sys
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QMainWindow, QGraphicsScene
from PyQt5.QtGui import QBrush
from PyQt5.QtCore import Qt, QThread , pyqtSignal
from PyQt5 import uic
from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import QGraphicsPixmapItem
import socket
import math
import time
import json

SIZE = 650
TIME_TICK = 0.1
class TimingGenerator(QThread):
    # Define a signal that will be emitted with the new x and y values
    time_tick_signal = QtCore.pyqtSignal()

    def __init__(self):
        super().__init__()
        self.running = True

    def run(self):
        while self.running:
            self.time_tick_signal.emit()
            time.sleep(TIME_TICK)

    def stop(self):
        self.running = False


class SocketListener(QThread):
    droneUpdate = QtCore.pyqtSignal(tuple)
    def __init__(self,port):
        super().__init__()
        self.port = port
        self.running = True
    def run(self):
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            s.bind(("localhost", self.port))
            print(f"GUI Listening on UDP port {self.port}")
            while self.running:
                data, addr = s.recvfrom(1024)
                if not data:
                    break
                data = data.decode("utf-8")
                drone_tuple = tuple(self.to_number(x) for x in data.split(","))
                self.droneUpdate.emit(drone_tuple)

    @staticmethod
    def to_number(x):
        try:
            return int(x)
        except ValueError:
            return float(x)

    def terminate(self):
        self.running = False
        super().terminate()


class DoubleStream(QtCore.QObject):
    textAvailable = QtCore.pyqtSignal(str)
    def __init__(self, text_edit):
        super().__init__()
        self.text_edit = text_edit
        self.original_stdout = sys.stdout

    def write(self, text):
        self.original_stdout.write(text)  # Write to the original stdout
        self.textAvailable.emit(text)  # Emit a signal with the stdout text

    def flush(self):
        # This can be left empty or you can implement any flush logic you need.
        pass

class DroneGridApp(QMainWindow):
    def __init__(self, port):
        super().__init__()
        self.GUI_folder = os.path.dirname(__file__)
        # Load the UI at runtime
        uic.loadUi(f'{self.GUI_folder}/gui_frontend.ui', self)
        # Redirect stdout to the text edit
        sys.stdout = DoubleStream(self.console)
        self.port = port
        self.scene = None
        self.draw_scene()
        self.drone_icon_size = 10
        self.drones = {}
        self.connect_signals()

        # Start the socket listener
        self.RT_socket_listener = SocketListener(self.port)
        self.RT_socket_listener.droneUpdate.connect(self.update_drone)
        self.RT_socket_listener.start()

        # start timing generator
        self.TG_Thread = TimingGenerator()
        self.TG_Thread.time_tick_signal.connect(self.move_drones_slot)
        self.TG_Thread.start()



    def closeEvent(self, event):
        if hasattr(self, 'move_thread'):
            self.move_thread.stop()
            self.move_thread.wait()  # Wait for the thread to finish.

        if hasattr(self, 'RT_socket_listener'):
            self.RT_socket_listener.terminate()
            self.RT_socket_listener.wait()  # Wait for the thread to finish.
        sys.stdout = sys.stdout.original_stdout
        del self.scene

        super().closeEvent(event)

    def draw_scene(self):
        # Set up QGraphicsScene
        self.scene = QGraphicsScene()
        self.scene.setSceneRect(0, 0, SIZE, SIZE)
        # Load the map pixmap
        map_pixmap = QPixmap(f"{self.GUI_folder}/map.jpg")

        # Create a pixmap item with the loaded pixmap
        map_item = QGraphicsPixmapItem(map_pixmap)

        # Adjust the pixmap to fit the scene size (OPTIONAL)
        map_item.setPixmap(map_pixmap.scaled(SIZE, SIZE, Qt.KeepAspectRatio))

        # Add the pixmap item to the scene
        self.scene.addItem(map_item)
        self.graphicsView.setScene(self.scene)
        # add 4 borderlines:
        self.scene.addLine(0, 0, SIZE, 0)  # From (0, 0) to (SIZE, 0)
        self.scene.addLine(0, 0, 0, SIZE)  # From (0, 0) to (0, SIZE)
        self.scene.addLine(SIZE, 0, SIZE, SIZE)  # From (SIZE, 0) to (SIZE, SIZE)
        self.scene.addLine(0, SIZE, SIZE, SIZE)  # From (0, SIZE) to (SIZE, SIZE)
        # Drawing the vertical line
        self.scene.addLine(SIZE / 2, 0, SIZE / 2, SIZE)  # From (SIZE/2, 0) to (SIZE/2, SIZE)
        # Drawing the horizontal line
        self.scene.addLine(0, SIZE / 2, SIZE, SIZE / 2)  # From (0, SIZE/2) to (SIZE, SIZE/2)

    def connect_signals(self):
        self.launchButton.clicked.connect(self.launch_button_clicked)


    def launch_button_clicked(self):
        pass

    def add_drone(self, drone_id):
        triangle = create_triangle(self.drone_icon_size)
        drone_item = QtWidgets.QGraphicsPolygonItem(triangle)
        drone_item.setBrush(QBrush(Qt.red))
        drone_item.setPos(10000, 10000)
        star = create_star(self.drone_icon_size)
        wp_item = QtWidgets.QGraphicsPolygonItem(star)
        wp_item.setBrush(QBrush(Qt.blue))
        wp_item.setPos(10000, 10000)
        self.scene.addItem(wp_item)
        self.scene.addItem(drone_item)
        self.drones[drone_id] = {'obj':drone_item, 'movement':(0,0), 'location':(0,0), 'wp':wp_item}

    def move_drone(self, drone_id, x, y, angle, speed):
        if drone_id in self.drones:
            drone_item = self.drones[drone_id]['obj']

            drone_item.setRotation(-angle+90)
            drone_item.setPos(x, convert_to_scene_coordinates(y)) # move the (0,0) point to the center of screen
            self.drones[drone_id]['movement'] = (angle, speed)
            self.drones[drone_id]['location'] = (x, y)
        else:
            self.add_drone(drone_id)
            self.move_drone(drone_id, x, y, angle, speed)

    def move_wp(self, drone_id, wp_x, wp_y):
        if drone_id in self.drones:
            wp_item = self.drones[drone_id]['wp']
            wp_item.setPos(wp_x, convert_to_scene_coordinates(wp_y)) # move the (0,0) point to the center of screen





    # slots

    @QtCore.pyqtSlot(str)
    def update_console(self, text):
        self.console.append(text)

    @QtCore.pyqtSlot(tuple)
    def update_drone(self,drone_tuple):
        drone_id, x, y, theta, speed, wp_x, wp_y, wp_theta = drone_tuple
        self.move_drone(drone_id, x, y, theta,speed)
        self.move_wp(drone_id, wp_x, wp_y)

    @QtCore.pyqtSlot()
    def move_drones_slot(self):
        for drone_id, drone in self.drones.items():
            angle, speed = drone['movement']
            theta = angle * math.pi / 180
            x = drone['location'][0] + speed * math.cos(theta)
            y = drone['location'][1] + speed * math.sin(theta)
            self.move_drone(drone_id, x, y, angle, speed)


def convert_to_scene_coordinates(y):
    return -y + SIZE


def create_triangle(size):
    """Create a triangle pointing upwards."""
    return QtGui.QPolygonF([
        QtCore.QPointF(0, -size) ,       # Top center
        QtCore.QPointF(size/2, size/2),   # Bottom right
        QtCore.QPointF(-size/2, size/2),  # Bottom left
    ])

def create_star(size):
    """Create a four-point star."""
    return QtGui.QPolygonF([
        QtCore.QPointF(0, -size),       # Top
        QtCore.QPointF(0, size),        # Bottom
        QtCore.QPointF(size, 0),        # Right
        QtCore.QPointF(-size, 0),       # Left
        QtCore.QPointF(0, -size),       # Return to Top
    ])


if __name__ == '__main__':
    # argument parser
    parser = argparse.ArgumentParser(description='Drone GUI')
    parser.add_argument('--port', type=int, default=8000, help='port number for communication with gui_pc erlang node',
                        required=False)
    args = parser.parse_args()
    app = QApplication(sys.argv)
    window = DroneGridApp(args.port)
    window.show()
    # window.move_drone(1, SIZE/2, SIZE/2, 0, 1)



    sys.exit(app.exec_())
