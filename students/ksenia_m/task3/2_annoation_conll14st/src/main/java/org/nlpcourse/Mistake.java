package org.nlpcourse;

public class Mistake {
    private int nid;
    private int pid;
    private int startToken;
    private int endToken;
    private String type;
    private String correction;


    public int getNid() {
        return nid;
    }

    public void setNid(int nid) {
        this.nid = nid;
    }

    public int getPid() {
        return pid;
    }

    public void setPid(int pid) {
        this.pid = pid;
    }

    public int getStartToken() {
        return startToken;
    }

    public void setStartToken(int startToken) {
        this.startToken = startToken;
    }

    public int getEndToken() {
        return endToken;
    }

    public void setEndToken(int endToken) {
        this.endToken = endToken;
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

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;

        Mistake mistake = (Mistake) o;

        if (nid != mistake.nid)
            return false;
        if (pid != mistake.pid)
            return false;
        if (startToken != mistake.startToken)
            return false;
        if (endToken != mistake.endToken)
            return false;
        if (type != null ? !type.equals(mistake.type) : mistake.type != null)
            return false;
        return correction != null ? correction.equals(mistake.correction) : mistake.correction == null;
    }

    @Override
    public int hashCode() {
        int result = nid;
        result = 31 * result + pid;
        result = 31 * result + startToken;
        result = 31 * result + endToken;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (correction != null ? correction.hashCode() : 0);
        return result;
    }
}
