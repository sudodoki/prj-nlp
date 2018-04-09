package org.nlpcourse;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.SequenceInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class AnnotationReader {
    private String sourcePath;

    private Set<Mistake> mistakes = new LinkedHashSet<>();
    private Mistake current;

    public AnnotationReader(String sourcePath) {
        this.sourcePath = sourcePath;
    }

    public Set<Mistake> parseAndGetMistakes() throws ParserConfigurationException, SAXException, IOException {
        newParser().parse(getWrapperStream(sourcePath), new SAXHandler());
        return mistakes;
    }

    private SAXParser newParser() throws ParserConfigurationException, SAXException {
        return SAXParserFactory.newInstance().newSAXParser();
    }

    private SequenceInputStream getWrapperStream(String sourcePath) throws FileNotFoundException {
        return new SequenceInputStream(Collections.enumeration(Arrays.asList(
                new ByteArrayInputStream("<root>".getBytes()),
                new FileInputStream(sourcePath),
                new ByteArrayInputStream("</root>".getBytes())
        )));
    }

    private class SAXHandler extends DefaultHandler {

        private String currentQName;

        @Override
        public void startElement(String uri, String localName, String qName, Attributes atts)
                throws SAXException {
            if ("MISTAKE".equalsIgnoreCase(qName)) {
                current = new Mistake();
               current.setStartToken(Integer.valueOf(atts.getValue("start_token")));
               current.setEndToken(Integer.valueOf(atts.getValue("end_token")));
               current.setNid(Integer.valueOf(atts.getValue("nid")));
               current.setPid(Integer.valueOf(atts.getValue("pid")));
            }
            currentQName = qName;
        }

        @Override
        public void characters(char[] ch, int start, int length) throws SAXException {
            if ("TYPE".equals(currentQName)) {
                current.setType(new String(ch, start, length));
            } else if ("CORRECTION".equals(currentQName)) {
                current.setCorrection(new String(ch, start, length));
            }
        }

        @Override
        public void endElement(String uri, String localName, String qName) throws SAXException {
            currentQName = null;
            if ("MISTAKE".equalsIgnoreCase(qName)) {
                mistakes.add(current);
            }
        }

    }
}
