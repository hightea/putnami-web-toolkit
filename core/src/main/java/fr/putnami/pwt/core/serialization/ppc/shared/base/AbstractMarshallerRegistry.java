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
package fr.putnami.pwt.core.serialization.ppc.shared.base;

import com.google.common.collect.Maps;

import java.util.Map;

import fr.putnami.pwt.core.serialization.ppc.shared.MarshallerRegistry;
import fr.putnami.pwt.core.serialization.ppc.shared.SerializationException;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.ArrayListMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.BigDecimalMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.BigIntegerMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.BooleanMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.ByteMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.CharacterMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.DateMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.DoubleMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.FloatMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.HashMapMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.HashSetMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.IdentityHashMapMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.IntegerMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.LinkedHashMapMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.LinkedHashSetMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.LinkedListMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.LongMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.Marshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.ShortMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.StringMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.TreeMapMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.TreeSetMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.VectorMarshaller;
import fr.putnami.pwt.core.serialization.ppc.shared.marshaller.VoidMarshaller;

public class AbstractMarshallerRegistry implements MarshallerRegistry {

	protected final Map<Object, Marshaller<?>> registry = Maps.newHashMap();

	@Override
	public <T> Marshaller<T> findMarshaller(Class<T> clazz) {
		Class type = clazz;
		Marshaller<T> marshaller = null;
		while (marshaller == null && type != null) {
			marshaller = (Marshaller<T>) registry.get(type);
			type = type.getSuperclass();
		}
		return marshaller;
	}

	@Override
	public <T> Marshaller<T> findMarshaller(String className) {
		Marshaller<T> marshaller = (Marshaller<T>) registry.get(className);
		if (marshaller == null) {
			throw new SerializationException(className + " doesnt have any marshaller.");
		}
		return marshaller;
	}

	protected void registerDefault() {
		register(new ArrayListMarshaller());
		register(new BigDecimalMarshaller());
		register(new BigIntegerMarshaller());
		register(new BooleanMarshaller());
		register(new ByteMarshaller());
		register(new CharacterMarshaller());
		register(new DateMarshaller());
		register(new DoubleMarshaller());
		// register(new EmptyListMarshaller());
		// register(new EmptyMapMarshaller());
		// register(new EmptySetMarshaller());
		// register(new EnumMapMarshaller());
		register(new FloatMarshaller());
		register(new HashMapMarshaller());
		register(new HashSetMarshaller());
		register(new IdentityHashMapMarshaller());
		register(new IntegerMarshaller());
		register(new LinkedHashMapMarshaller());
		register(new LinkedHashSetMarshaller());
		register(new LinkedListMarshaller());
		register(new LongMarshaller());
		// register(new MathContextMarshaller());
		register(new ShortMarshaller());
		register(new StringMarshaller());
		// register(new SingletonListMarshaller());
		// register(new StackTraceElementMarshaller());
		register(new TreeMapMarshaller());
		register(new TreeSetMarshaller());
		register(new VectorMarshaller());
		register(new VoidMarshaller());
	}

	protected void register(Marshaller marshaller) {
		if (registry.containsKey(marshaller.getTypeName())) {
			throw new SerializationException(marshaller.getTypeName() + " already registered");
		}
		if (registry.containsKey(marshaller.getType())) {
			throw new SerializationException(marshaller.getType() + " already registered");
		}
		registry.put(marshaller.getType().getName(), marshaller);
		registry.put(marshaller.getType(), marshaller);
		registry.put(marshaller.getTypeName(), marshaller);
	}

}
