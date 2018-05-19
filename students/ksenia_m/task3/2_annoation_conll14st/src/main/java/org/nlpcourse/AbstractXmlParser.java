package org.nlpcourse;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;
import java.util.Arrays;
import java.util.Collections;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

public abstract class AbstractXmlParser {
    protected String sourcePath;

    public AbstractXmlParser(String sourcePath) {
        this.sourcePath = sourcePath;
    }

    public abstract void parse() throws ParserConfigurationException, SAXException, IOException;

    protected InputStream getWrapperStream(String sourcePath) throws FileNotFoundException {
        return new SequenceInputStream(Collections.enumeration(Arrays.asList(
                new ByteArrayInputStream("<root>".getBytes()),
                new FileInputStream(sourcePath),
                new ByteArrayInputStream("</root>".getBytes())
        )));
    }
}
