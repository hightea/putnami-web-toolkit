package fr.putnami.pwt.core.serialization.ppc.server;

import fr.putnami.pwt.core.serialization.ppc.shared.PpcSerializer;

public class PpcServerSerializer extends PpcSerializer {

	public PpcServerSerializer() {
		setMarshallerRegistry(new MarshallerServerRegistry());
	}
}
