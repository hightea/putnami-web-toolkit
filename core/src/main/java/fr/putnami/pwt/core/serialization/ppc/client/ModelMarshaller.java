package fr.putnami.pwt.core.serialization.ppc.client;

import com.google.common.collect.Lists;

import java.util.Collections;
import java.util.List;

import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.PropertyDescription;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcReader;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcWriter;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.AbstractMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.util.PpcUtils;

public class ModelMarshaller<T> extends AbstractMarshaller<T> {

	private final Model<T> model;
	private final List<String> properties;

	public ModelMarshaller(Model<T> model) {
		this.model = model;
		this.properties = Lists.newArrayList(model.getPropertyNames());
		Collections.sort(properties);
	}

	@Override
	public void marshal(T bean, PpcWriter writer) {
		for (String property : properties) {
			PropertyDescription propertyDescription = model.getProperty(property);
			Object value = model.get(bean, property);
			if (boolean.class.equals(propertyDescription.getClazz())) {
				writer.write(Boolean.TRUE.equals(value));
			} else if (byte.class.equals(propertyDescription.getClazz())) {
				writer.write((byte) (value == null ? 0 : value));
			} else if (char.class.equals(propertyDescription.getClazz())) {
				writer.write((char) (value == null ? 0 : value));
			} else if (double.class.equals(propertyDescription.getClazz())) {
				writer.write((double) (value == null ?  0 :  value));
			} else if (float.class.equals(propertyDescription.getClazz())) {
				writer.write((float)(value == null ? 0 :  value));
			} else if (int.class.equals(propertyDescription.getClazz())) {
				writer.write((int)(value == null ?  0 : value));
			} else if (long.class.equals(propertyDescription.getClazz())) {
				writer.write( (long) (value == null ?0 :value));
			} else if (short.class.equals(propertyDescription.getClazz())) {
				writer.write((short) (value == null ? 0 : value));
			} else if (value == null) {
				writer.writeNull();
			} else {
				writer.write(value);
			}
		}
	}

	@Override
	public T unmarshal(PpcReader reader) {
		T instance = newInstance();
		for (String property : properties) {
			PropertyDescription propertyDescription = model.getProperty(property);
			if (boolean.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readBoolean());
			} else if (byte.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readByte());
			} else if (char.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readChar());
			} else if (double.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readDouble());
			} else if (float.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readFloat());
			} else if (int.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readInt());
			} else if (long.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readLong());
			} else if (short.class.equals(propertyDescription.getClazz())) {
				model.set(instance, property, reader.readShort());
			} else {
				model.set(instance, property, reader.readObject());
			}
		}
		return instance;
	}

	@Override
	public String getTypeName() {
		return model.getBeanType().getName();
	}

	@Override
	public Class<?> getType() {
		return model.getBeanType();
	}

	@Override
	public T newInstance() {
		return model.newInstance();
	}

	@Override
	public boolean writeType(PpcWriter writer, Integer id) {
		writer.write(getTypeName() + PpcUtils.SEPARATOR_TYPE_REF + id);
		return true;
	}

}
