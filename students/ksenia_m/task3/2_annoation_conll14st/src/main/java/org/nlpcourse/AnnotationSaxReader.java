package org.nlpcourse;

import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.nlpcourse.model.Mistake;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class AnnotationSaxReader extends AbstractXmlParser {

    private Set<Mistake> mistakes = new LinkedHashSet<>();

    private Mistake current;

    public AnnotationSaxReader(String sourcePath) {
        super(sourcePath);
    }

    public void parse() throws ParserConfigurationException, SAXException, IOException {
        newParser().parse(getWrapperStream(sourcePath), new SAXHandler());
    }

    private SAXParser newParser() throws ParserConfigurationException, SAXException {
        return SAXParserFactory.newInstance().newSAXParser();
    }

    private class SAXHandler extends DefaultHandler {

        private String currentQName;

        @Override
        public void startElement(String uri, String localName, String qName, Attributes atts)
                throws SAXException {
            if ("MISTAKE".equalsIgnoreCase(qName)) {
                current = new Mistake();
               current.setStartOff(Integer.valueOf(atts.getValue("start_token")));
               current.setEndOff(Integer.valueOf(atts.getValue("end_token")));
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
