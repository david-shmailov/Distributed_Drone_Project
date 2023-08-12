import argparse
import os.path
import sys
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QMainWindow, QGraphicsScene, QGraphicsView, QWidget, QGraphicsEllipseItem
from PyQt5.QtGui import QBrush, QPen, QColor, QPainter, QPixmap
from PyQt5.QtCore import Qt, QThread , pyqtSignal, QSize, QPointF
from PyQt5 import uic
from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import QGraphicsPixmapItem
import socket
import math
import time
import json

SIZE = 650
TIME_TICK = 0.1
SEARCH_RADIUS = 20



class XWidget(QWidget):
    def __init__(self, parent=None):
        super(XWidget, self).__init__(parent)
        self.size = 20
        self.setFixedSize(QSize(self.size, self.size))  # Size of the widget
        self.setAttribute(Qt.WA_TranslucentBackground, True)  # Set background to be transparent


    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setPen(QPen(QColor("black"), 5))  # Adjust pen width for thickness

        # Drawing from top-left to bottom-right
        painter.drawLine(QPointF(0, 0), QPointF(self.width(), self.height()))

        # Drawing from top-right to bottom-left
        painter.drawLine(QPointF(self.width(), 0), QPointF(0, self.height()))



class CustomGraphicsView(QGraphicsView):
    mouse_clicked = pyqtSignal(float, float)
    def __init__(self, parent=None):
        super().__init__(parent)

    def mousePressEvent(self, event):
        scene_point = self.mapToScene(event.pos())
        x, y = scene_point.x(), scene_point.y()
        self.mouse_clicked.emit(x, y)


class TimingGenerator(QThread):
    # Define a signal that will be emitted with the new x and y values
    time_tick_signal = pyqtSignal()

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
    gs_established = pyqtSignal(str)
    droneUpdate = pyqtSignal(tuple)
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
                tokens = data.split(",")
                if tokens[0].strip() == "drone":
                    drone_tuple = tuple(self.to_number(x) for x in tokens[1:])
                    self.droneUpdate.emit(drone_tuple)
                elif tokens[0].strip() == "establish_comm":
                    self.gs_established.emit(tokens[1])
                else:
                    print(f"Unknown message: {data}")


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
    textAvailable = pyqtSignal(str)
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
    def __init__(self, in_port, out_port):
        super().__init__()
        self.GUI_folder = os.path.dirname(__file__)
        # Load the UI at runtime
        uic.loadUi(f'{self.GUI_folder}/gui_frontend.ui', self)
        # Redirect stdout to the text edit
        sys.stdout = DoubleStream(self.console)
        self.in_port = in_port
        self.out_port = out_port
        self.scene = None
        self.draw_scene()
        self.drone_icon_size = 10
        self.drones = {}
        self.waypoints = []
        self.targets = []
        self.plotting = False
        self.targeting = False
        self.connect_signals()
        self.search_radius = SEARCH_RADIUS
        # Start the socket listener
        self.RT_socket_listener = SocketListener(self.in_port)
        self.RT_socket_listener.droneUpdate.connect(self.update_drone)
        self.RT_socket_listener.gs_established.connect(self.add_gs_slot)
        self.RT_socket_listener.start()

        # start timing generator
        self.TG_Thread = TimingGenerator()
        self.TG_Thread.time_tick_signal.connect(self.move_drones_slot)
        self.TG_Thread.start()




    def connect_signals(self):
        self.launch_button.clicked.connect(self.launch_drones_slot)
        self.set_waypoints_button.clicked.connect(self.set_waypoints_slot)
        self.graphicsView.mouse_clicked.connect(self.mouse_clicked_slot)
        self.plot_button.clicked[bool].connect(self.plot_slot)
        self.place_targets_button.clicked[bool].connect(self.set_targets_slot)


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

        self.scene.addLine(SIZE*0.25, 0, SIZE*0.25, SIZE)  # From (0, 0) to (SIZE, SIZE)
        self.scene.addLine(SIZE*0.50, 0, SIZE*0.50, SIZE)  # From (0, 0) to (SIZE, SIZE)
        self.scene.addLine(SIZE*0.75, 0, SIZE*0.75, SIZE)  # From (0, 0) to (SIZE, SIZE)


    def add_drone(self, drone_id):
        triangle = create_triangle(self.drone_icon_size)
        drone_item = QtWidgets.QGraphicsPolygonItem(triangle)
        drone_item.setBrush(QBrush(Qt.red))
        drone_item.setPos(10000, 10000)
        drone_label = QtWidgets.QGraphicsTextItem(str(drone_id))
        drone_label.setPos(10000, 10000 + self.drone_icon_size)
        star = create_star(self.drone_icon_size*1.2)
        next_wp_item = QtWidgets.QGraphicsPolygonItem(star)
        next_wp_item.setBrush(QBrush(Qt.blue))
        next_wp_item.setPos(10000, 10000)

        circle_item = QGraphicsEllipseItem(10000 - self.search_radius, 10000 - self.search_radius,
                                           2 * self.search_radius, 2 * self.search_radius)
        self.scene.addItem(circle_item)
        self.scene.addItem(drone_label)
        self.scene.addItem(next_wp_item)
        self.scene.addItem(drone_item)
        self.drones[drone_id] = {'obj':drone_item,'label':drone_label, 'circle':circle_item,
                                 'movement':(0,0), 'location':(0,0), 'wp':next_wp_item}

    def move_drone(self, drone_id, x, y, angle, speed):
        if drone_id in self.drones:
            drone_item = self.drones[drone_id]['obj']

            drone_item.setRotation(-angle+90)
            drone_item.setPos(x, convert_y_coordinates(y)) # move the (0,0) point to the center of screen
            drone_label = self.drones[drone_id]['label']
            drone_label.setPos(x, convert_y_coordinates(y) + self.drone_icon_size) # move the (0,0) point to the center of screen
            circle_item = self.drones[drone_id]['circle']
            circle_item.setRect(x - self.search_radius, convert_y_coordinates(y) - self.search_radius,
                                2 * self.search_radius, 2 * self.search_radius)
            self.drones[drone_id]['movement'] = (angle, speed)
            self.drones[drone_id]['location'] = (x, y)
        else:
            self.add_drone(drone_id)
            self.move_drone(drone_id, x, y, angle, speed)

    def move_next_wp(self, drone_id, wp_x, wp_y):
        if drone_id > 0:
            return
        if drone_id in self.drones:
            next_wp_item = self.drones[drone_id]['wp']
            next_wp_item.setPos(wp_x, convert_y_coordinates(wp_y)) # move the (0,0) point to the center of screen



    # slots
    @QtCore.pyqtSlot(str)
    def add_gs_slot(self, gs_name):
        self.gs_list_combo_box.addItem(str(gs_name))
        self.launch_button.setEnabled(True)

    @QtCore.pyqtSlot(str)
    def update_console(self, text):
        self.console.append(text)

    @QtCore.pyqtSlot(tuple)
    def update_drone(self,drone_tuple):
        drone_id, x, y, theta, speed, wp_x, wp_y, wp_theta = drone_tuple
        self.move_drone(drone_id, x, y, theta,speed)
        self.move_next_wp(drone_id, wp_x, wp_y)

    @QtCore.pyqtSlot()
    def move_drones_slot(self):
        for drone_id, drone in self.drones.items():
            angle, speed = drone['movement']
            theta = angle * math.pi / 180
            x = drone['location'][0] + speed * math.cos(theta)
            y = drone['location'][1] + speed * math.sin(theta)
            self.move_drone(drone_id, x, y, angle, speed)

    @QtCore.pyqtSlot()
    def launch_drones_slot(self):
        GS = self.gs_list_combo_box.currentText()
        Number_of_drones = self.num_of_drones_box.value()
        dic = {'launch_drones': f'{GS} {Number_of_drones}'}
        self.launch_button.setEnabled(False)
        self.send_data_to_erl_node(str(dic))


    @QtCore.pyqtSlot(float, float)
    def mouse_clicked_slot(self, x, y):
        if self.plotting:
            self.add_waypoint(x, y)
        elif self.targeting:
            self.add_target(x, y)



    @QtCore.pyqtSlot()
    def set_waypoints_slot(self):
        # waypoints = [(SIZE/ 4, SIZE / 4), (SIZE / 4, 3 * SIZE / 4), (3 * SIZE / 4, 3 * SIZE / 4), (3 * SIZE / 4, SIZE / 4)]
        for wp in self.waypoints:
            wp = (wp[0], convert_y_coordinates(wp[1]))
            dic = {'add_waypoint': wp}
            self.send_data_to_erl_node(str(dic))
        self.send_data_to_erl_node(str({'set_waypoints': ''}))

    @QtCore.pyqtSlot(bool)
    def plot_slot(self, checked):
        self.plotting = checked
        if checked:
            self.plot_button.setStyleSheet("background-color: green")
        else:
            self.plot_button.setStyleSheet("background-color: white")
            if self.waypoints: # if not empty
                self.set_waypoints_button.setEnabled(True)
                self.close_route()

    @QtCore.pyqtSlot(bool)
    def set_targets_slot(self, checked):
        self.targeting = checked
        if checked:
            self.place_targets_button.setStyleSheet("background-color: green")
        else:
            self.place_targets_button.setStyleSheet("background-color: white")


    def add_target(self, x, y):
        if not self.targeting:
            return
        x_widget = XWidget()
        centered_x = x - x_widget.width() / 2
        centered_y = y - x_widget.height() / 2

        self.graphicsView.scene().addWidget(x_widget).setPos(QPointF(centered_x, centered_y))
        self.targets.append((x, y))
        target = (x, convert_y_coordinates(y))
        self.send_data_to_erl_node(str({'add_target': target}))

    def add_waypoint(self, x, y):
        if not self.plotting:
            return
        radius = 10
        waypoint = QtWidgets.QGraphicsEllipseItem(x - radius / 2, y - radius / 2, radius, radius)
        brush = QtGui.QBrush(QtGui.QColor(0, 255, 0))  # Green color
        waypoint.setBrush(brush)
        self.graphicsView.scene().addItem(waypoint)
        if self.waypoints: # if not empty
            prev_wp = self.waypoints[-1]
            self.scene.addLine(prev_wp[0], prev_wp[1], x, y, QtGui.QPen(Qt.black, 2, Qt.SolidLine))
        self.waypoints.append((x, y))

    def close_route(self):
        first_wp = self.waypoints[0]
        last_wp = self.waypoints[-1]
        if first_wp != last_wp:
            self.scene.addLine(first_wp[0], first_wp[1], last_wp[0], last_wp[1], QtGui.QPen(Qt.black, 2, Qt.SolidLine))

    def send_data_to_erl_node(self, data):
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
            s.sendto(data.encode('utf-8'), ('localhost', self.out_port))


def convert_y_coordinates(y):
    """
    converts the y coordinate from the real world to the graphics world
    and vice versa
    :param y:
    :return:
    """
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
        QtCore.QPointF(size/4, -size/4),
        QtCore.QPointF(size, 0),        # Right
        QtCore.QPointF(size / 4, size / 4),
        QtCore.QPointF(0, size),        # Bottom
        QtCore.QPointF(-size / 4, size / 4),  # Right
        QtCore.QPointF(-size, 0),       # Left
        QtCore.QPointF(-size / 4, -size / 4),  # Right
        QtCore.QPointF(0, -size),       # Return to Top
    ])


if __name__ == '__main__':
    # argument parser
    parser = argparse.ArgumentParser(description='Drone GUI')
    parser.add_argument('--in_port', type=int, default=8000, help='port number for receiving from gui_pc erlang node',
                        required=False)
    parser.add_argument('--out_port', type=int, default=8001,
                        help='port number for sending to gui_pc erlang node',
                        required=False)
    args = parser.parse_args()
    app = QApplication(sys.argv)
    window = DroneGridApp(args.in_port, args.out_port)
    window.show()
    # window.move_drone(1, SIZE/2, SIZE/2, 0, 1)



    sys.exit(app.exec_())
