/*
 * Copyright 2011 Google Inc. Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in
 * writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied. See the License for the specific language governing permissions and limitations under the License.
 */
package com.google.gwt.uibinder.client;

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;

/**
 * This file is derived from the original LazyDomElement. This version doesn't remove the 'id' attribute.
 */
public class LazyDomElement<T extends Element> {

	private T element;
	private final String domId;

	public LazyDomElement(String domId) {
		this.domId = domId;
	}

	public T get() {
		if (element == null) {
			element = Document.get().getElementById(domId).<T> cast();
			if (element == null) {
				throw new RuntimeException("Cannot find element with id \"" + domId + "\". Perhaps it is not attached to the document body.");
			}
		}
		return element;
	}
}
