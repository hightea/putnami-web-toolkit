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
package fr.putnami.pwt.core.editor.client.impl;

import com.google.gwt.thirdparty.guava.common.collect.Lists;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Path;

public class SimpleError implements Error {

	private final Editor editor;
	private final String message;
	private final Object value;
	private final Object[] parameters;

	private boolean consumed;
	private Path path;

	public SimpleError(Editor editor, String message, Object value, Object[] parameters) {
		this.editor = editor;
		this.message = message;
		this.value = value;
		this.parameters = parameters == null ? new Object[] {} : parameters;
	}

	public SimpleError(Editor editor, String message, Object value) {
		this.editor = editor;
		this.message = message;
		this.value = value;
		this.parameters = new Object[] {};
	}

	@Override
	public Editor getEditor() {
		return this.editor;
	}

	@Override
	public String getMessageKey() {
		return this.message;
	}

	@Override
	public Object[] getParameters() {
		return Lists.newArrayList(parameters).toArray();
	}

	@Override
	public Object getValue() {
		return this.value;
	}

	@Override
	public boolean isConsumed() {
		return this.consumed;
	}

	@Override
	public void consume() {
		this.consumed = true;
	}

	@Override
	public Path getPath() {
		return path;
	}

	public void setPath(Path path) {
		this.path = path;
	}

}
