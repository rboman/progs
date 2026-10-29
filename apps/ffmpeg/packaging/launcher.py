"""PyInstaller entry point; pipx continues to use the same main function."""
import os
import sys

# A windowed executable has no standard streams. Keep libraries that flush them
# working; Window redirects stdout into its existing GUI journal afterwards.
for stream in ("stdin", "stdout", "stderr"):
    if getattr(sys, stream) is None:
        setattr(sys, stream, open(os.devnull, "r" if stream == "stdin" else "w"))

from rb_ffmpeg.ffmpeg_GUI import main

if __name__ == "__main__":
    main()

