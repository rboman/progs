import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')
from PyQt5.QtWidgets import QApplication
from rb_ffmpeg.notices import NoticesDialog


class NoticesTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        package = Path(self.temp.name)
        self.resources = package / 'resources'
        self.resources.mkdir()
        self.overview = self.resources / 'THIRD_PARTY.txt'
        self.overview.write_text('Overview and sources', encoding='utf-8')
        self.license = self.resources / 'notices' / 'component' / 'LICENSE.txt'
        self.license.parent.mkdir(parents=True)
        self.license.write_text('License line\n' * 20000, encoding='utf-8')
        self.manual = package / '_vendor' / 'ffmpeg' / 'doc' / 'ffmpeg.html'
        self.manual.parent.mkdir(parents=True)
        self.manual.write_text('<html>' + ('manual ' * 500000) + '</html>', encoding='utf-8')

    def test_only_selected_notice_is_read_and_scrolling_reaches_end(self):
        reads = []
        read_text = Path.read_text

        def track(file, *args, **kwargs):
            reads.append(file)
            return read_text(file, *args, **kwargs)

        with patch.object(Path, 'read_text', track):
            dialog = NoticesDialog(resources=self.resources)
            self.addCleanup(dialog.close)
            self.assertEqual(reads, [self.overview])
            self.assertNotIn(self.manual, dialog.files)
            self.assertEqual(dialog.reader.toPlainText(), 'Overview and sources')
            dialog.show()
            dialog.documents.setCurrentRow(dialog.files.index(self.license))
            self.app.processEvents()
            self.assertEqual(reads, [self.overview, self.license])
            scroll = dialog.reader.verticalScrollBar()
            self.assertGreater(scroll.maximum(), 0)
            scroll.setValue(scroll.maximum())
            self.app.processEvents()
            self.assertEqual(scroll.value(), scroll.maximum())
            dialog.documents.setCurrentRow(0)
            self.assertEqual(dialog.reader.toPlainText(), 'Overview and sources')
            self.assertEqual(dialog.reader.verticalScrollBar().value(), 0)

    def test_html_documentation_opens_externally(self):
        dialog = NoticesDialog(resources=self.resources)
        self.addCleanup(dialog.close)
        with patch('rb_ffmpeg.notices.QDesktopServices.openUrl', return_value=True) as open_url:
            dialog.documentation_button.click()
        self.assertEqual(Path(open_url.call_args.args[0].toLocalFile()), self.manual)

    def test_unreadable_notice_does_not_break_dialog(self):
        dialog = NoticesDialog(resources=self.resources)
        self.addCleanup(dialog.close)
        self.license.unlink()
        dialog.documents.setCurrentRow(dialog.files.index(self.license))
        self.assertIn('Cannot read this notice', dialog.reader.toPlainText())


if __name__ == '__main__':
    unittest.main()
