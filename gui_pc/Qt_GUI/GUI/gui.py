import os.path
import sys
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QMainWindow, QGraphicsScene
from PyQt5.QtGui import QBrush
from PyQt5.QtCore import Qt, QThread , pyqtSignal
from PyQt5 import uic
import socket
import math
import time

SIZE = 650

class MoveDroneThread(QThread):
    # Define a signal that will be emitted with the new x and y values
    move_signal = QtCore.pyqtSignal()

    def __init__(self, drone_id):
        super().__init__()
        self.drone_id = drone_id
        self.running = True

    def run(self):
        while self.running:
            self.move_signal.emit()
            time.sleep(0.05)

    def stop(self):
        self.running = False


class SocketListener(QThread):
    messageReceived = QtCore.pyqtSignal(str)

    def run(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(("localhost", 8000))
            s.listen()
            conn, addr = s.accept()
            with conn:
                data = conn.recv(1024)
                self.messageReceived.emit(data.decode())


class DoubleStream:
    def __init__(self, text_edit):
        self.text_edit = text_edit
        self.original_stdout = sys.stdout

    def write(self, text):
        self.original_stdout.write(text)  # Write to the original stdout
        self.text_edit.append(text)  # Append text to QTextEdit

    def flush(self):
        # This can be left empty or you can implement any flush logic you need.
        pass

class DroneGridApp(QMainWindow):
    def __init__(self):
        super().__init__()
        GUI_folder = os.path.dirname(__file__)
        # Load the UI at runtime
        uic.loadUi(f'{GUI_folder}/gui_frontend.ui', self)
        # Redirect stdout to the text edit
        sys.stdout = DoubleStream(self.console)

        self.scene = None
        self.draw_scene()
        self.icon_size = 10
        self.drones = {}
        self.connect_signals()

        # Start the socket listener
        self.RT_socket_listener = SocketListener()
        self.RT_socket_listener.messageReceived.connect(self.showMessage)
        self.RT_socket_listener.start()


        # for testing
        # Initial angle in radians
        self.theta = 0

        # Define the radius of the circle
        self.radius = 50

        # Central point of the circle
        self.center = QtCore.QPointF(SIZE/2, SIZE/2)

    def closeEvent(self, event):
        if hasattr(self, 'move_thread'):
            self.move_thread.stop()
            self.move_thread.wait()  # Wait for the thread to finish.

        if hasattr(self, 'RT_socket_listener'):
            self.RT_socket_listener.terminate()
            self.RT_socket_listener.wait()  # Wait for the thread to finish.
        sys.stdout = sys.stdout.original_stdout
        del self.drones
        del self.scene

        super().closeEvent(event)

    def draw_scene(self):
        # Set up QGraphicsScene
        self.scene = QGraphicsScene()
        self.scene.setSceneRect(0, 0, SIZE, SIZE)
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
        self.pushButton.clicked.connect(self.button_clicked)


    def button_clicked(self):
        x = self.spinBox_x.value()
        y = self.spinBox_y.value()
        self.move_drone("drone_1", x, y)

    def add_drone(self, drone_id, x, y):
        triangle = create_triangle(self.icon_size)
        drone_item = QtWidgets.QGraphicsPolygonItem(triangle)
        drone_item.setBrush(QBrush(Qt.red))
        drone_item.setPos(x, y)
        self.scene.addItem(drone_item)
        self.drones[drone_id] = drone_item

    def move_drone(self, drone_id, x, y):
        if drone_id in self.drones:
            drone_item = self.drones[drone_id]
            # Calculate the angle of rotation based on movement direction
            dx = x - drone_item.x()
            dy = y - drone_item.y()
            angle = math.atan2(dy, dx) * 180 / math.pi
            drone_item.setRotation(angle+90)
            drone_item.setPos(x, y)
        else:
            self.add_drone(drone_id, x, y)

    def fly_drone(self):
        self.move_thread = MoveDroneThread("drone_1")
        self.move_thread.move_signal.connect(self.move_drone_slot)
        self.move_thread.start()

    @QtCore.pyqtSlot(str)
    def showMessage(self,message):
        print(message)

    @QtCore.pyqtSlot()
    def move_drone_slot(self):
        # Calculate new x and y using trigonometry
        x = self.center.x() + self.radius * math.cos(self.theta)
        y = self.center.y() + self.radius * math.sin(self.theta)

        # Move the drone to the new position
        self.move_drone("drone_1", x, y)

        # Increment the angle. This determines the speed of the circular motion.
        # You can adjust the value added to self.theta to make it move faster/slower.
        self.theta += 0.1



def create_triangle(size):
    """Create a triangle pointing upwards."""
    return QtGui.QPolygonF([
        QtCore.QPointF(0, -size) ,       # Top center
        QtCore.QPointF(size/2, size/2),   # Bottom right
        QtCore.QPointF(-size/2, size/2),  # Bottom left
    ])


if __name__ == '__main__':
    app = QApplication(sys.argv)
    window = DroneGridApp()
    window.show()

    # Example usage:
    window.add_drone("drone_1", 2, 3)
    window.move_drone("drone_1", 5, 5)
    window.add_drone("drone_2", 7, 7)
    window.fly_drone()


    sys.exit(app.exec_())
