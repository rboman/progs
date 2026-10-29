"""Read individual license/source notices without laying out FFmpeg's manuals."""
from pathlib import Path

from PyQt5.QtCore import Qt, QUrl
from PyQt5.QtGui import QDesktopServices
from PyQt5.QtWidgets import (
    QDialog, QDialogButtonBox, QHBoxLayout, QListWidget, QMessageBox,
    QPlainTextEdit, QPushButton, QSplitter, QVBoxLayout,
)


class NoticesDialog(QDialog):
    def __init__(self, parent=None, resources=None):
        super().__init__(parent)
        resources = Path(resources) if resources is not None else Path(__file__).resolve().parent / 'resources'
        vendor = resources.parent / '_vendor' / 'ffmpeg'
        self.setWindowTitle('Licenses / Sources')
        self.resize(1050, 650)

        self.files = [resources / 'THIRD_PARTY.txt']
        labels = ['Overview / Sources']
        for file in sorted((resources / 'notices').rglob('*')):
            if file.is_file():
                self.files.append(file)
                relative = file.relative_to(resources / 'notices')
                parts = list(relative.parts[1:])
                if parts and parts[0].endswith('.dist-info'):
                    parts.pop(0)
                parts = [part for part in parts if part not in ('licenses', '_vendor', relative.parts[0])]
                labels.append(' / '.join([relative.parts[0]] + parts))
        for name in ('LICENSE', 'README.txt'):
            file = vendor / name
            if file.is_file():
                self.files.append(file)
                labels.append('FFmpeg / ' + name)

        layout = QVBoxLayout(self)
        splitter = QSplitter(Qt.Horizontal, self)
        self.documents = QListWidget(splitter)
        self.documents.addItems(labels)
        for index, file in enumerate(self.files):
            self.documents.item(index).setToolTip(str(file))
        self.reader = QPlainTextEdit(splitter)
        self.reader.setReadOnly(True)
        self.reader.setUndoRedoEnabled(False)
        # Long metadata lines must not trigger expensive reflow on resize/scroll.
        self.reader.setLineWrapMode(QPlainTextEdit.NoWrap)
        splitter.setSizes([280, 770])
        splitter.setStretchFactor(1, 1)
        layout.addWidget(splitter)

        footer = QHBoxLayout()
        documentation = vendor / 'doc' / 'ffmpeg.html'
        self.documentation_button = QPushButton('Open FFmpeg documentation', self)
        self.documentation_button.setEnabled(documentation.is_file())
        self.documentation_button.clicked.connect(lambda: self.open_document(documentation))
        footer.addWidget(self.documentation_button)
        footer.addStretch()
        buttons = QDialogButtonBox(QDialogButtonBox.Close, parent=self)
        buttons.rejected.connect(self.reject)
        footer.addWidget(buttons)
        layout.addLayout(footer)

        self.documents.currentRowChanged.connect(self.load_notice)
        self.documents.setCurrentRow(0)

    def load_notice(self, index):
        if not 0 <= index < len(self.files):
            return
        try:
            text = self.files[index].read_text(encoding='utf-8', errors='replace')
        except OSError as error:
            text = 'Cannot read this notice:\n%s' % error
        self.reader.setPlainText(text)
        self.reader.verticalScrollBar().setValue(0)
        self.reader.horizontalScrollBar().setValue(0)

    def open_document(self, file):
        if not QDesktopServices.openUrl(QUrl.fromLocalFile(str(file))):
            QMessageBox.warning(self, 'Documentation', 'Cannot open:\n%s' % file)
