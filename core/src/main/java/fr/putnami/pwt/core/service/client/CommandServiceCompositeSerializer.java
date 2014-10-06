/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.service.client;

import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.client.rpc.SerializationStreamReader;
import com.google.gwt.user.client.rpc.SerializationStreamWriter;
import com.google.gwt.user.client.rpc.impl.Serializer;

import java.util.Collection;

public class CommandServiceCompositeSerializer implements Serializer {

	private Collection<Serializer> serializers;

	public CommandServiceCompositeSerializer(Collection<Serializer> serializers) {
		this.serializers = serializers;
	}

	@Override
	public String getSerializationSignature(Class<?> clazz) {
		for (Serializer serializer : this.serializers) {
			String signature = serializer.getSerializationSignature(clazz);
			if (signature != null) {
				return signature;
			}
		}
		return null;
	}

	@Override
	public Object instantiate(SerializationStreamReader stream, String typeSignature)
			throws SerializationException {
		for (Serializer serializer : this.serializers) {
			try {
				Object o = serializer.instantiate(stream, typeSignature);
				if (o != null) {
					return o;
				}
			} catch (SerializationException e) {
			}
		}
		throw new SerializationException(typeSignature);
	}

	@Override
	public void serialize(SerializationStreamWriter stream, Object instance, String typeSignature)
			throws SerializationException {
		for (Serializer serializer : this.serializers) {
			try {
				serializer.serialize(stream, instance, typeSignature);
				return;
			} catch (SerializationException e) {
			}
		}
		throw new SerializationException(typeSignature);
	}

	@Override
	public void deserialize(SerializationStreamReader stream, Object instance, String typeSignature)
			throws SerializationException {
		for (Serializer serializer : this.serializers) {
			try {
				serializer.deserialize(stream, instance, typeSignature);
				return;
			} catch (SerializationException e) {
			}
		}
		throw new SerializationException(typeSignature);
	}

}
