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
package fr.putnami.pwt.core.editor.client;

import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import java.util.Iterator;
import java.util.List;

import fr.putnami.pwt.core.editor.client.Path.PathElement;

public class Path implements Iterable<PathElement> {

	public static class PathElement {

		private final String elementName;
		private final Integer indexKey;

		public PathElement(String elementName, Integer indexKey) {
			super();
			this.elementName = elementName;
			this.indexKey = indexKey;
		}

		public String getElementName() {
			return this.elementName;
		}

		public Integer getIndexKey() {
			return this.indexKey;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append(this.elementName);
			if (this.indexKey != null) {
				sb.append("[");
				sb.append(this.indexKey);
				sb.append("]");
			}
			return sb.toString();
		}

		@Override
		public int hashCode() {
			return Objects.hashCode(this.elementName, this.indexKey);
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof PathElement) {
				PathElement other = (PathElement) obj;
				return Objects.equal(this.elementName, other.elementName)
					&& Objects.equal(this.indexKey, other.indexKey);
			}
			return false;
		}
	}

	public static final String SEPARATOR_PATH = ".";
	public static final String ROOT_PATH = "";

	final List<PathElement> elements = Lists.newLinkedList();

	public void add(PathElement element) {
		if (element != null) {
			this.elements.add(element);
		}
	}

	@Override
	public Iterator<PathElement> iterator() {
		return Iterables.<PathElement> unmodifiableIterable(this.elements).iterator();
	}

	public int size() {
		return this.elements.size();
	}

	public boolean isEmpty() {
		return this.elements.isEmpty();
	}

	public PathElement get(int index) {
		return this.elements.size() > index ? this.elements.get(index) : null;
	}

	public Path subPath(int start) {
		return this.subPath(start, Integer.MAX_VALUE);
	}

	public Path subPath(int start, int end) {
		Path path = new Path();
		if (this.elements.size() > start) {
			for (int i = start; i < end && this.elements.size() > i; i++) {
				path.add(this.elements.get(i));
			}
		}
		return path;
	}

	public boolean isRoot() {
		return isEmpty();
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(this.elements);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Path) {
			Path other = (Path) obj;
			return Objects.equal(this.elements, other.elements);
		}
		return obj == null && this.isRoot();
	}

	@Override
	public String toString() {
		return Joiner.on(Path.SEPARATOR_PATH).join(this.elements);
	}
}
