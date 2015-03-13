package fr.putnami.pwt.core.serialization.ppc.server;

import com.ibm.icu.text.SimpleDateFormat;

import java.text.ParseException;
import java.util.Date;

import fr.putnami.pwt.core.serialization.ppc.shared.AbstractPpcTest;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcSerializer;

public class PpcServerTest extends AbstractPpcTest {

	private static PpcSerializer serializer;

	@Override
	public boolean isPureJava() {
		return true;
	}

	@Override
	protected PpcSerializer getSerializer() {
		if (serializer == null) {
			serializer = new PpcServerSerializer();
		}
		return serializer;
	}

	@Override
	protected Date parseDate(String text) {
		SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");
		try {
			return df.parse(text);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}
}
