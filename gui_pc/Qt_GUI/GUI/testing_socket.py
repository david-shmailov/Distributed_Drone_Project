import sys
from PyQt5.QtWidgets import QApplication, QLabel, QVBoxLayout, QWidget
from PyQt5.QtCore import QThread, pyqtSignal
import socket

class SocketListener(QThread):
    messageReceived = pyqtSignal(str)

    def run(self):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(("localhost", 8000))
            s.listen()
            conn, addr = s.accept()
            with conn:
                data = conn.recv(1024)
                self.messageReceived.emit(data.decode())

class App(QWidget):
    def __init__(self):
        super().__init__()
        self.title = 'PyQt5 Socket Listener'
        self.initUI()

    def initUI(self):
        self.setWindowTitle(self.title)
        layout = QVBoxLayout()

        self.label = QLabel("Waiting for message...")
        layout.addWidget(self.label)

        self.setLayout(layout)
        self.show()

        self.thread = SocketListener()
        self.thread.messageReceived.connect(self.showMessage)
        self.thread.start()

    def showMessage(self, message):
        self.label.setText(f"Received: {message}")

app = QApplication(sys.argv)
ex = App()
sys.exit(app.exec_())
