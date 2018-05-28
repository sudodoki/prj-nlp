package org.nlpcourse.model;

public class Mistake {
    private int startOff;
    private int endOff;
    private int startPar;
    private int endPar;
    private String type;
    private String correction;

    public int getStartOff() {
        return startOff;
    }

    public void setStartOff(int startOff) {
        this.startOff = startOff;
    }

    public int getEndOff() {
        return endOff;
    }

    public void setEndOff(int endOff) {
        this.endOff = endOff;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getCorrection() {
        return correction;
    }

    public void setCorrection(String correction) {
        this.correction = correction;
    }

    public int getStartPar() {
        return startPar;
    }

    public void setStartPar(int startPar) {
        this.startPar = startPar;
    }

    public int getEndPar() {
        return endPar;
    }

    public void setEndPar(int endPar) {
        this.endPar = endPar;
    }
}
