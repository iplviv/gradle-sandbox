package org.gradle;

import org.apache.commons.collections.list.GrowthList;

public class Address {
    private final String name;

    public Address(String name) {
        this.name = name;
        new GrowthList();
    }

    public String getName() {
        return name;
    }
}
