package fr.putnami.pwt.core.widget.client.util;


import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import java.util.regex.Pattern;

/**
 * The test Class for the UUID helper.
 */
public class UuidTest {

	private static final Pattern UUID_PATTERN = Pattern
		.compile("^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$");

	/**
	 * test that the {@link UUID#uuid()} method match the pattern
	 * "^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$".
	 */
	@Test
	public void uuid() {
		for (int i = 0; i < 100; i++) {
			String id = UUID.uuid();
			assertNotNull(id);
			assertTrue(UUID_PATTERN.matcher(id).matches());
		}
	}
}
