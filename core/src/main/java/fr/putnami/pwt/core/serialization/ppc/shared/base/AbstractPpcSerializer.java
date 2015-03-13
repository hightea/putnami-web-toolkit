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

import fr.putnami.pwt.core.serialization.ppc.shared.MarshallerRegistry;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcReader;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcSerializer;
import fr.putnami.pwt.core.serialization.ppc.shared.PpcWriter;
import fr.putnami.pwt.core.serialization.ppc.shared.SimpleReader;
import fr.putnami.pwt.core.serialization.ppc.shared.SimpleWriter;

public abstract class AbstractPpcSerializer implements PpcSerializer {

	private MarshallerRegistry marshallerRegistry;

	@Override
	public void setMarshallerRegistry(MarshallerRegistry marshallerRegistry) {
		this.marshallerRegistry = marshallerRegistry;
	}

	@Override
	public MarshallerRegistry getMarshallerRegistry() {
		return marshallerRegistry;
	}

	@Override
	public PpcReader newReader() {
		return new SimpleReader(marshallerRegistry);
	}

	@Override
	public PpcWriter newWriter() {
		return new SimpleWriter(marshallerRegistry);
	}

	@Override
	public <O> O deserialize(String payload) {
		PpcReader reader = newReader();
		reader.prepare(payload);
		return reader.readObject();
	}

	@Override
	public String serialize(Object object) {
		PpcWriter writer = newWriter();
		writer.write(object);
		return writer.flush();
	}
}
