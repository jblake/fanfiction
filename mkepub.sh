#!/bin/bash

set -e

EPUB="$1"
shift 1

BOOKID="$1"
shift 1

TITLE="$1"
shift 1

AUTHOR="$1"
shift 1

RAW="$(tempfile -p mk_ -s .epub)"

mkdir "${RAW}.src"

function cleanUp () {
  rm -rf "${RAW}" "${RAW}.src"
}

trap cleanUp EXIT

echo "application/epub+zip" > "${RAW}.src/mimetype"

rm -f "${RAW}"

( cd "${RAW}.src"; zip "${RAW}" -Z store -q mimetype )

mkdir "${RAW}.src/META-INF"
mkdir "${RAW}.src/OPS"

cat > "${RAW}.src/META-INF/container.xml" <<END
<?xml version="1.0" encoding="iso-8859-1" ?>

<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OPS/book.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>
END

cat > "${RAW}.src/OPS/book.opf" <<END
<?xml version="1.0" ?>

<package version="2.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="${BOOKID}">

  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>${TITLE}</dc:title>
    <dc:identifier id="BookId">${BOOKID}</dc:identifier>
    <dc:creator>${AUTHOR}</dc:creator>
  </metadata>

  <manifest>
    <item id="ncx" href="book.ncx" media-type="application/x-dtbncx+xml"/>
END

cat > "${RAW}.src/OPS/book.ncx" <<END
<?xml version="1.0" encoding="iso-8859-1"?>

<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">

<ncx version="2005-1" xmlns="http://www.daisy.org/z3986/2005/ncx/">

  <head>
    <meta name="dtb:uid" content="${BOOKID}" />
    <meta name="dtb:depth" content="1" />
    <meta name="dtb:totalPageCount" content="0" />
    <meta name="dtb:maxPageNumber" content="0" />
  </head>

  <docTitle><text>${TITLE}</text></docTitle>
  <docAuthor><text>${AUTHOR}</text></docAuthor>

  <navMap>
END

CHAPTERS="0"

while [ "$#" -gt 0 ]; do

  CHAPTERS="$((${CHAPTERS} + 1))"

  cat > "${RAW}.src/OPS/chapter${CHAPTERS}.xhtml" <<END
<?xml version="1.0" encoding="iso-8859-1" ?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <meta http-equiv="Content-Type" content="application/xhtml+xml; charset=iso-8859-1" />
    <title>${TITLE}</title>
  </head>
  <body>

END

  ./html2xhtml <(iconv -f utf-8 -t iso-8859-1//translit -c "$1") >> "${RAW}.src/OPS/chapter${CHAPTERS}.xhtml"

  cat >> "${RAW}.src/OPS/chapter${CHAPTERS}.xhtml" <<END

  </body>
</html>
END

  cat >> "${RAW}.src/OPS/book.opf" <<END
    <item id="chapter${CHAPTERS}" href="chapter${CHAPTERS}.xhtml" media-type="application/xhtml+xml" />
END

  cat >> "${RAW}.src/OPS/book.ncx" <<END
    <navPoint class="chapter" id="chapter${CHAPTERS}" playOrder="${CHAPTERS}">
      <navLabel><text>Chapter ${CHAPTERS}</text></navLabel>
      <content src="chapter${CHAPTERS}.xhtml" />
    </navPoint>
END

  shift 1

done

cat >> "${RAW}.src/OPS/book.opf" <<END
  </manifest>

  <spine toc="ncx">
END

for CHAPTER in $(seq 1 "${CHAPTERS}"); do
  cat >> "${RAW}.src/OPS/book.opf" <<END
    <itemref idref="chapter${CHAPTER}" />
END
done

cat >> "${RAW}.src/OPS/book.opf" <<END
  </spine>

</package>
END

cat >> "${RAW}.src/OPS/book.ncx" <<END
  </navMap>

</ncx>
END

( cd "${RAW}.src"; zip "${RAW}" -r -q META-INF OPS )

echo "  Post-processing with ebook-convert..."

ebook-convert "${RAW}" "${EPUB}" > /dev/null
