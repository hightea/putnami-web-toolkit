package fr.putnami.pwt.core.serialization.ppc.shared;

public interface PpcSerializer {

	void setMarshallerRegistry(MarshallerRegistry marshallerRegistry);

	MarshallerRegistry getMarshallerRegistry();

	PpcReader newReader();

	PpcWriter newWriter();

	<O> O deserialize(String payload);

	String serialize(Object object);


}
