package org.nlpcourse;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class Application {

    public static void main(String[] args) throws ParserConfigurationException, SAXException, IOException {
        if (args.length == 0) {
            throw new IllegalArgumentException("Expect path to *-pages-articles.xml");
        }
        try (PrintWriter printWriter = new PrintWriter("output.txt", "UTF-8")) {
            newParser().parse(new File(args[0]), new SAXHandler(printWriter));
        }
    }

    private static SAXParser newParser() throws ParserConfigurationException, SAXException {
        return SAXParserFactory.newInstance().newSAXParser();
    }

    private static class SAXHandler extends DefaultHandler {

        private static final String SYNONYMES = "{{S|synonymes}}";
        private static final Pattern SYN_PATTERN = Pattern.compile("\\[\\[(.+?)\\]\\]");
        private static final Pattern NEW_SECTION = Pattern.compile("={3,4} \\{\\{[^={}]+?\\}\\} ={3,4}");
        private static final String TEXT = "text";
        private String currentQName;
        private String currentTitle;
        private PrintWriter outputFile;
        private StringBuilder textBuffer;

        public SAXHandler(PrintWriter outputFile) {
            this.outputFile = outputFile;
        }

        @Override
        public void startElement(String uri, String localName, String qName, Attributes attributes)
                throws SAXException {
            currentQName = qName;
            if (TEXT.equals(currentQName)) {
                textBuffer = new StringBuilder();
            }
        }

        @Override
        public void endElement(String uri, String localName, String qName) throws SAXException {
            super.endElement(uri, localName, qName);
            if (TEXT.equals(currentQName)) {
                findSynonymsFor(currentTitle, textBuffer.toString());
            }
            currentQName = null;
        }

        @Override
        public void characters(char[] ch, int start, int length) throws SAXException {
            if ("title".equals(currentQName)) {
                currentTitle = new String(ch, start, length);
            } else if (TEXT.equals(currentQName)) {
                textBuffer.append(new String(ch, start, length));
            }
        }

        private void findSynonymsFor(String currentTitle, String text) {
            if (text.contains(SYNONYMES)) {
                String synSection = text.substring(text.indexOf(SYNONYMES) + SYNONYMES.length());
                Matcher matcher = NEW_SECTION.matcher(synSection);
                if (matcher.find()) {
                    synSection = synSection.substring(0, matcher.start());
                }
                int doubleLineInd = synSection.indexOf("\n\n");
                if (doubleLineInd > 0) {
                    synSection = synSection.substring(0, doubleLineInd);
                }
                matcher = SYN_PATTERN.matcher(synSection);
                List<String> synonyms = new ArrayList<>();
                while (matcher.find()) {
                    synonyms.add(matcher.group(1));
                }
                if (!synonyms.isEmpty()) {
                    outputFile.println(currentTitle + ":" + synonyms.stream().collect(Collectors.joining(", ")));
                }
            }
        }
    }
}
