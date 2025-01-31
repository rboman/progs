#include "export.h"
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cstdio>
#include <stdint.h>
#include <string>
#include <vector>
#include <iostream>
#include <map>
#include <cmath>
#include <cassert>

#ifdef USE_ZLIB
#include <zlib.h>
#else
#define Z_OK 0
#define uLong size_t
#define uLongf size_t
#endif

// converts zlib status to a human-readable string

std::string zlibstatus(int status)
{
#ifdef USE_ZLIB
    switch (status)
    {
    case Z_OK:
        return "Z_OK";
    case Z_BUF_ERROR:
        return "Z_BUF_ERROR";
    case Z_MEM_ERROR:
        return "Z_MEM_ERROR";
    case Z_STREAM_ERROR:
        return "Z_STREAM_ERROR";
    default:
        std::stringstream str;
        str << "Unknown (" << status << ")";
        return str.str();
    }
#else
    return "zlib missing";
#endif
}

bool isCpuLittleEndian()
{
    static const int __one__ = 1;
    static const bool ret = 1 == *(char *)(&__one__); // CPU endianness
    return ret;
}

// sends a vector of "doubles" in binary XML/format into filestream f
//  f: destination filestream
//  pos: the vector to be sent
//  usez: true if zlib should be used
//
// "limitation": the number of particles should be less than 2^31 = 2,147,483,648

size_t write_vectorXML(std::ofstream &f, std::vector<double> const &pos, bool usez)
{
    size_t written = 0;

    // convert doubles to floats
    std::vector<float> buffer(pos.size());
    for (size_t i = 0; i < pos.size(); ++i)
        buffer[i] = (float)pos[i];

    if (!usez)
    {
        // data block size
        uint32_t sz = (uint32_t)pos.size() * sizeof(float);
        f.write((char *)&sz, sizeof(uint32_t));
        written += sizeof(uint32_t);
        // data
        f.write((char *)&buffer[0], sz);
        written += sz;
    }
    else
    {
        uLong sourcelen = (uLong)pos.size() * sizeof(float);
        uLongf destlen = uLongf(sourcelen * 1.001) + 12; // see doc
        char *destbuffer = new char[destlen];
#ifdef USE_ZLIB
        int status = compress2((Bytef *)destbuffer, &destlen,
                               (Bytef *)&(buffer[0]), sourcelen, Z_DEFAULT_COMPRESSION);
#else
        int status = Z_OK + 1;
#endif
        if (status != Z_OK)
        {
            std::cout << "ERROR: zlib Error status=" << zlibstatus(status) << "\n";
        }
        else
        {
            // std::cout << "block of size " << sourcelen << " compressed to " << destlen << '\n';
            //  blocks description
            uint32_t nblocks = 1;
            f.write((char *)&nblocks, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t srclen = (uint32_t)sourcelen;
            f.write((char *)&srclen, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t lastblocklen = 0;
            f.write((char *)&lastblocklen, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t szblocki = (uint32_t)destlen;
            f.write((char *)&szblocki, sizeof(uint32_t));
            written += sizeof(uint32_t);
            // data
            f.write(destbuffer, destlen);
            written += destlen;
        }

        delete[] destbuffer;
    }

    return written;
}

// sends a vector of "integers" in binary XML/format into filestream f
//  f: destination filestream
//  pos: the vector to be sent
//  usez: true if zlib should be used

// TODO: merge both versions using a template?

size_t write_vectorXML(std::ofstream &f, std::vector<int> const &pos, bool usez)
{
    size_t written = 0;

    if (!usez)
    {
        // data block size
        uint32_t sz = (uint32_t)pos.size() * sizeof(int);
        f.write((char *)&sz, sizeof(uint32_t));
        written += sizeof(uint32_t);
        // data
        f.write((char *)&pos[0], sz);
        written += sz;
    }
    else
    {
        uLong sourcelen = (uLong)pos.size() * sizeof(int);
        uLongf destlen = uLongf(sourcelen * 1.001) + 12; // see doc
        char *destbuffer = new char[destlen];
#ifdef USE_ZLIB
        int status = compress2((Bytef *)destbuffer, &destlen,
                               (Bytef *)&(pos[0]), sourcelen, Z_DEFAULT_COMPRESSION);
#else
        int status = Z_OK + 1;
#endif
        if (status != Z_OK)
        {
            std::cout << "ERROR: zlib Error status=" << zlibstatus(status) << "\n";
        }
        else
        {
            // std::cout << "block of size " << sourcelen << " compressed to " << destlen << '\n';
            //  blocks description
            uint32_t nblocks = 1;
            f.write((char *)&nblocks, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t srclen = (uint32_t)sourcelen;
            f.write((char *)&srclen, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t lastblocklen = 0;
            f.write((char *)&lastblocklen, sizeof(uint32_t));
            written += sizeof(uint32_t);
            uint32_t szblocki = (uint32_t)destlen;
            f.write((char *)&szblocki, sizeof(uint32_t));
            written += sizeof(uint32_t);
            // data
            f.write(destbuffer, destlen);
            written += destlen;
        }

        delete[] destbuffer;
    }

    return written;
}

// export results to paraview (VTK polydata - XML fomat)
//   filename: file name without vtk extension
//   pos:     positions (vector of size 3*number of particles)
//   step:    time step number
//   scalars: scalar fields defined on particles (map linking [field name] <=> [vector of results v1, v2, v3, v4, ...]
//   vectors: vector fields defined on particles (map linking [field name] <=> [vector of results v1x, v1y, v1z, v2x, v2y, ...]

// see http://www.vtk.org/Wiki/VTK_XML_Formats
//     https://docs.vtk.org/en/latest/design_documents/VTKFileFormats.html 

void export_particles(std::string const &filename,
                      int step,
                      std::vector<double> const &pos,
                      std::map<std::string, std::vector<double> *> const &scalars,
                      std::map<std::string, std::vector<double> *> const &vectors,
                      bool verb)
{
#if defined(USE_ZLIB)
    bool usez = true;
#else
    bool usez = false;
#endif

    int nbp = (int)pos.size() / 3;
    assert(pos.size() == (size_t)nbp * 3); // should be multiple of 3

    // build file name + stepno + vtk extension
    std::stringstream s;
    s << filename << std::setw(8) << std::setfill('0') << step << ".vtp";
    std::stringstream s2;
    s2 << filename << std::setw(8) << std::setfill('0') << step << ".vtp.tmp";

    // open file
    if (verb)
        std::cout << "writing results to " << s.str() << " (compression:" <<  (usez? "on" : "off") << ")\n";
    std::ofstream f(s.str().c_str(), std::ios::binary | std::ios::out);
    std::ofstream f2(s2.str().c_str(), std::ios::binary | std::ios::out); // temp binary file
    f << std::scientific;

    size_t offset = 0;
    // header
    // std::cout << "\twriting header" << std::endl;
    f << "<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"";
    f << (isCpuLittleEndian() ? "LittleEndian" : "BigEndian") << "\" ";
    f << "header_type=\"UInt32\" "; // UInt64 should be better
    if (usez)
        f << "compressor=\"vtkZLibDataCompressor\" ";
    f << ">\n";
    f << "  <PolyData>\n";
    f << "    <Piece NumberOfPoints=\"" << nbp << "\" ";
    f << "NumberOfVerts=\"" << nbp << "\" ";
    f << "NumberOfLines=\"0\" ";
    f << "NumberOfStrips=\"0\" ";
    f << "NumberOfPolys=\"0\">\n";

    // -------------------------------------------------------------------------
    // std::cout << "\twriting <PointData>" << std::endl;
    f << "      <PointData>\n";
    // scalar fields
    for (auto const &p : scalars)
    {
        assert(p.second->size() == (size_t)nbp);
        f << "        <DataArray type=\"Float32\" ";
        f << " Name=\"" << p.first << "\" ";
        f << " format=\"appended\" ";
        f << " RangeMin=\"0\" ";
        f << " RangeMax=\"1\" ";
        f << " offset=\"" << offset << "\" />\n";
        offset += write_vectorXML(f2, *p.second, usez);
    }
    // vector fields
    for (auto const &p : vectors)
    {
        assert(p.second->size() == 3 * (size_t)nbp);
        f << "        <DataArray type=\"Float32\" ";
        f << " Name=\"" << p.first << "\" ";
        f << " NumberOfComponents=\"3\" ";
        f << " format=\"appended\" ";
        f << " RangeMin=\"0\" ";
        f << " RangeMax=\"1\" ";
        f << " offset=\"" << offset << "\" />\n";
        offset += write_vectorXML(f2, *p.second, usez);
    }
    f << "      </PointData>\n";

    // -------------------------------------------------------------------------
    f << "      <CellData>\n";
    f << "      </CellData>\n";

    // -------------------------------------------------------------------------
    // std::cout << "\twriting <Points>" << std::endl;
    f << "      <Points>\n";
    f << "        <DataArray type=\"Float32\" ";
    f << " Name=\"Points\" ";
    f << " NumberOfComponents=\"3\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, pos, usez);
    f << "      </Points>\n";
    // -------------------------------------------------------------------------
    // std::cout << "\twriting <Verts>" << std::endl;
    f << "      <Verts>\n";
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"connectivity\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"" << nbp - 1 << "\" ";
    f << " offset=\"" << offset << "\" />\n";

    std::vector<int> connectivity(nbp); // <= hard to avoid if zlib is used
    for (int i = 0; i < nbp; ++i)
        connectivity[i] = i;
    offset += write_vectorXML(f2, connectivity, usez);

    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"offsets\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"1\" ";
    f << " RangeMax=\"" << nbp << "\" ";
    f << " offset=\"" << offset << "\" />\n";

    // reuse "connectivity" for offsets
    for (int i = 0; i < nbp; ++i)
        connectivity[i] = i + 1;
    offset += write_vectorXML(f2, connectivity, usez);

    f << "      </Verts>\n";

    std::vector<double> empty;
    // -------------------------------------------------------------------------
    // std::cout << "\twriting <Lines>" << std::endl;
    f << "      <Lines>\n";
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"connectivity\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);

    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"offsets\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);
    f << "      </Lines>\n";

    // -------------------------------------------------------------------------
    // std::cout << "\twriting <Strips>" << std::endl;
    f << "      <Strips>\n";
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"connectivity\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"offsets\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);
    f << "      </Strips>\n";

    // -------------------------------------------------------------------------
    // std::cout << "\twriting <Polys>" << std::endl;
    f << "      <Polys>\n";
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"connectivity\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);
    f << "        <DataArray type=\"Int32\" ";
    f << " Name=\"offsets\" ";
    f << " format=\"appended\" ";
    f << " RangeMin=\"0\" ";
    f << " RangeMax=\"1\" ";
    f << " offset=\"" << offset << "\" />\n";
    offset += write_vectorXML(f2, empty, usez);
    f << "      </Polys>\n";

    f2.close();

    // -------------------------------------------------------------------------
    f << "    </Piece>\n";
    f << "  </PolyData>\n";
    // -------------------------------------------------------------------------
    // std::cout << "\twriting <AppendedData>" << std::endl;
    f << "  <AppendedData encoding=\"raw\">\n";
    f << "    _";

    // copy temp binary file
    std::ifstream f3(s2.str().c_str(), std::ios::binary | std::ios::in);
    f << f3.rdbuf();
    f3.close();
    // remove temp file
    std::remove(s2.str().c_str());

    f << "  </AppendedData>\n";
    f << "</VTKFile>\n";

    f.close();
}
