package org.gradle;

import org.junit.Test;
import static org.junit.Assert.*;

public class AddressTest {
    @Test
    public void canConstructAddress() {
        Address address = new Address("1st street");
        assertEquals("1st street", address.getName());
    }
}
