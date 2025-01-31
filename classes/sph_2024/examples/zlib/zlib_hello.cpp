// Description: A simple example of using zlib to compress a string and write it to a file.
#include <zlib.h>
#include <string>
#include <iostream>

void writeCompressedFile(const std::string& filename, const std::string& content) {
    gzFile gzfile = gzopen(filename.c_str(), "wb");
    if (!gzfile) {
        std::cerr << "Unable to open file for writing.\n";
        return;
    }

    int written = gzwrite(gzfile, content.c_str(), content.size());
    if (written == 0) {
        int errNo = 0;
        std::cerr << "Error during writing: " << gzerror(gzfile, &errNo) << "\n";
    }

    gzclose(gzfile);
}

int main() {
    std::string filename = "compressed.gz";
    std::string content = "This is a test string to be compressed and written into a file.";
    writeCompressedFile(filename, content);
    return 0;
}
