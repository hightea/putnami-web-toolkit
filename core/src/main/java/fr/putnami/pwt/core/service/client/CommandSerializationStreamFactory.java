/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.service.client;

import com.google.gwt.user.client.rpc.SerializationException;
import com.google.gwt.user.client.rpc.SerializationStreamFactory;
import com.google.gwt.user.client.rpc.SerializationStreamReader;
import com.google.gwt.user.client.rpc.SerializationStreamWriter;
import com.google.gwt.user.client.rpc.impl.ClientSerializationStreamReader;
import com.google.gwt.user.client.rpc.impl.ClientSerializationStreamWriter;
import com.google.gwt.user.client.rpc.impl.Serializer;

public class CommandSerializationStreamFactory implements SerializationStreamFactory {

	private static final String SERIALIZATION_POLICY = "__";

	private final Serializer serializer;
	private final String moduleBaseURL;

	public CommandSerializationStreamFactory(Serializer serializer, String moduleBaseURL) {
		super();
		this.serializer = serializer;
		this.moduleBaseURL = moduleBaseURL;
	}

	@Override
	public SerializationStreamWriter createStreamWriter() {
		ClientSerializationStreamWriter clientSerializationStreamWriter = new ClientSerializationStreamWriter(
				this.serializer, this.moduleBaseURL, CommandSerializationStreamFactory.SERIALIZATION_POLICY);
		clientSerializationStreamWriter.prepareToWrite();

		return clientSerializationStreamWriter;
	}

	@Override
	public SerializationStreamReader createStreamReader(String encoded) throws SerializationException {
		ClientSerializationStreamReader clientSerializationStreamReader =
				new ClientSerializationStreamReader(this.serializer);
		String encodedResponse = encoded;
		if (encoded.startsWith("//OK") || encodedResponse.startsWith("//EX")) {
			encodedResponse = encodedResponse.substring(4);
		}
		clientSerializationStreamReader.prepareToRead(encodedResponse);
		return clientSerializationStreamReader;
	}

}
