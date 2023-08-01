import sys
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QApplication, QMainWindow, QGraphicsScene
from PyQt5.QtGui import QBrush
from PyQt5.QtCore import Qt
from PyQt5 import uic
from PyQt5.QtCore import QThread
import math
import time

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

def create_triangle(size):
    """Create a triangle pointing upwards."""
    return QtGui.QPolygonF([
        QtCore.QPointF(0, -size) ,       # Top center
        QtCore.QPointF(size/2, size/2),   # Bottom right
        QtCore.QPointF(-size/2, size/2),  # Bottom left
    ])

class DroneGridApp(QMainWindow):
    def __init__(self):
        super().__init__()

        # Load the UI at runtime
        uic.loadUi('gui_frontend.ui', self)

        # Set up QGraphicsScene
        self.scene = QGraphicsScene()
        SIZE = 650
        self.scene.setSceneRect(0, 0, SIZE, SIZE)
        self.graphicsView.setScene(self.scene)
        # add 4 border lines:
        self.scene.addLine(0, 0, SIZE, 0)  # From (0, 0) to (SIZE, 0)
        self.scene.addLine(0, 0, 0, SIZE)  # From (0, 0) to (0, SIZE)
        self.scene.addLine(SIZE, 0, SIZE, SIZE)  # From (SIZE, 0) to (SIZE, SIZE)
        self.scene.addLine(0, SIZE, SIZE, SIZE)  # From (0, SIZE) to (SIZE, SIZE)



        # Drawing the vertical line
        self.scene.addLine(SIZE/2, 0, SIZE/2, SIZE)  # From (SIZE/2, 0) to (SIZE/2, SIZE)
        # Drawing the horizontal line
        self.scene.addLine(0, SIZE/2, SIZE, SIZE/2)  # From (0, SIZE/2) to (SIZE, SIZE/2)
        self.icon_size = 10
        # Define a grid size
        self.grid_size = 20
        self.drones = {}
        self.connect_signals()

        # Initial angle in radians
        self.theta = 0

        # Define the radius of the circle
        self.radius = 50

        # Central point of the circle
        self.center = QtCore.QPointF(SIZE/2, SIZE/2)

    def connect_signals(self):
        self.pushButton.clicked.connect(self.button_clicked)

    def button_clicked(self):
        x = self.spinBox_x.value()
        y = self.spinBox_y.value()
        self.move_drone("drone_1", x, y)

    def add_drone(self, id, x, y):
        triangle = create_triangle(self.icon_size)
        drone_item = QtWidgets.QGraphicsPolygonItem(triangle)
        drone_item.setBrush(QBrush(Qt.red))
        drone_item.setPos(x, y)
        self.scene.addItem(drone_item)
        self.drones[id] = drone_item

    def move_drone(self, id, x, y):
        if id in self.drones:
            drone_item = self.drones[id]
            # Calculate the angle of rotation based on movement direction
            dx = x - drone_item.x()
            dy = y - drone_item.y()
            angle = math.atan2(dy, dx) * 180 / math.pi
            drone_item.setRotation(angle+90)
            drone_item.setPos(x, y)
        else:
            self.add_drone(id, x, y)

    def fly_drone(self):
        self.move_thread = MoveDroneThread("drone_1")
        self.move_thread.move_signal.connect(self.move_drone_slot)
        self.move_thread.start()

    @QtCore.pyqtSlot()
    # def move_drone_slot(self):
    #     current_pos = self.drones["drone_1"].pos()
    #     self.move_drone("drone_1", current_pos.x() + 1, current_pos.y() + 1)

    def move_drone_slot(self):
        # Calculate new x and y using trigonometry
        x = self.center.x() + self.radius * math.cos(self.theta)
        y = self.center.y() + self.radius * math.sin(self.theta)

        # Move the drone to the new position
        self.move_drone("drone_1", x, y)

        # Increment the angle. This determines the speed of the circular motion.
        # You can adjust the value added to self.theta to make it move faster/slower.
        self.theta += 0.1

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
