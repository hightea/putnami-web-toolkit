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
package fr.putnami.pwt.core.service.server.service;

import com.google.common.collect.Sets;
import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.server.rpc.SerializationPolicy;
import com.google.gwt.user.server.rpc.impl.SerializabilityUtil;
import com.google.gwt.user.server.rpc.impl.TypeNameObfuscator;

import java.io.Serializable;
import java.util.Set;

public final class CommandSerializationPolicy extends SerializationPolicy
	implements TypeNameObfuscator {

	private static final String ELISION_ERROR =
		"Type name elision in RPC payloads is only supported if the RPC whitelist file is used.";

	private static final Set<Class<? extends Serializable>> JRE_BLACKLIST = Sets.newHashSet(
		java.lang.ArrayStoreException.class, java.lang.AssertionError.class, java.lang.Boolean.class,
		java.lang.Byte.class, java.lang.Character.class, java.lang.Class.class,
		java.lang.ClassCastException.class, java.lang.Double.class, java.lang.Error.class,
		java.lang.Float.class, java.lang.IllegalArgumentException.class,
		java.lang.IllegalStateException.class, java.lang.IndexOutOfBoundsException.class,
		java.lang.Integer.class, java.lang.Long.class, java.lang.NegativeArraySizeException.class,
		java.lang.NullPointerException.class, java.lang.Number.class,
		java.lang.NumberFormatException.class, java.lang.Short.class,
		java.lang.StackTraceElement.class, java.lang.String.class, java.lang.StringBuffer.class,
		java.lang.StringIndexOutOfBoundsException.class, java.lang.UnsupportedOperationException.class,
		java.util.ArrayList.class, java.util.ConcurrentModificationException.class,
		java.util.Date.class, java.util.EmptyStackException.class, java.util.EventObject.class,
		java.util.HashMap.class, java.util.HashSet.class, java.util.MissingResourceException.class,
		java.util.NoSuchElementException.class, java.util.Stack.class,
		java.util.TooManyListenersException.class, java.util.Vector.class);

	private static CommandSerializationPolicy instance;

	public static CommandSerializationPolicy get() {
		if (CommandSerializationPolicy.instance == null) {
			CommandSerializationPolicy.instance = new CommandSerializationPolicy();
		}
		return CommandSerializationPolicy.instance;
	}

	private CommandSerializationPolicy() {
	}

	@Override
	public String getClassNameForTypeId(String id) throws SerializationException {
		throw new SerializationException(CommandSerializationPolicy.ELISION_ERROR);
	}

	@Override
	public String getTypeIdForClass(Class<?> clazz) throws SerializationException {
		throw new SerializationException(CommandSerializationPolicy.ELISION_ERROR);
	}

	@Override
	public boolean shouldDeserializeFields(Class<?> clazz) {
		return this.isFieldSerializable(clazz);
	}

	@Override
	public boolean shouldSerializeFields(Class<?> clazz) {
		return this.isFieldSerializable(clazz);
	}

	@Override
	public void validateDeserialize(Class<?> clazz) throws SerializationException {
		if (!this.isInstantiable(clazz)) {
			throw new SerializationException("Type '" + clazz.getName()
				+ "' was not assignableJRE_BLACKSET to '" + IsSerializable.class.getName()
				+ "' and did not have a custom field serializer. "
				+ "For security purposes, this type will not be deserialized.");
		}
	}

	@Override
	public void validateSerialize(Class<?> clazz) throws SerializationException {
		if (!this.isInstantiable(clazz)) {
			throw new SerializationException("Type '" + clazz.getName() + "' was not assignable to '"
				+ IsSerializable.class.getName() + "' and did not have a custom field serializer."
				+ "For security purposes, this type will not be serialized.");
		}
	}

	private boolean isFieldSerializable(Class<?> clazz) {
		if (this.isInstantiable(clazz)) {
			return true;
		}
		if (Serializable.class.isAssignableFrom(clazz)) {
			return !CommandSerializationPolicy.JRE_BLACKLIST.contains(clazz);
		}
		return false;
	}

	private boolean isInstantiable(Class<?> clazz) {
		if (clazz.isPrimitive()) {
			return true;
		}
		if (clazz.isEnum()) {
			return true;
		}
		if (Throwable.class.isAssignableFrom(clazz)) {
			return true;
		}
		if (clazz.isArray()) {
			return this.isInstantiable(clazz.getComponentType());
		}
		if (IsSerializable.class.isAssignableFrom(clazz)) {
			return true;
		}
		if (Serializable.class.isAssignableFrom(clazz)) {
			return true;
		}
		return SerializabilityUtil.hasCustomFieldSerializer(clazz) != null;
	}

}
