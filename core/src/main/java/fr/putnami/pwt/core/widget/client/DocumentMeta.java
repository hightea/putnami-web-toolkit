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
package fr.putnami.pwt.core.widget.client;

import com.google.common.base.Strings;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.HeadElement;
import com.google.gwt.dom.client.MetaElement;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.dom.client.SpanElement;
import com.google.gwt.event.logical.shared.AttachEvent;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractWidget;

/**
 * The DocumentMeta is a widget that alter a meta tag in the document head section
 * <p>
 * This widget changes or creates the first meta tag with the matching name when this widget is
 * loaded. When the widget unload event is fired, the tag is reverted to the original state.
 * </p>
 *
 */
public class DocumentMeta extends AbstractWidget {

	/**
	 * The PageAttachHandler handles the attach event and alter the meta tag.
	 */
	private class PageAttachHandler implements AttachEvent.Handler {
		private String fromContent;

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * com.google.gwt.event.logical.shared.AttachEvent.Handler#onAttachOrDetach(com.google.gwt.event
		 * .logical.shared.AttachEvent)
		 */
		@Override
		public void onAttachOrDetach(AttachEvent event) {
			if (event.isAttached()) {
				if (content != null) {
					MetaElement metaTag = DocumentMeta.getDescriptionTag(name, true);
					fromContent = Strings.emptyToNull(metaTag.getContent());
					metaTag.setContent(content);
				}
			} else {
				MetaElement metaTag = DocumentMeta.getDescriptionTag(name, true);
				if (fromContent != null) {
					metaTag.setContent(fromContent);
				} else {
					metaTag.removeFromParent();
				}
			}
		}
	}

	private final String name;
	private String content;

	/**
	 * Instantiates a new document meta tag.
	 *
	 * @param name the name attribute
	 */
	@UiConstructor
	public DocumentMeta(String name) {
		super(SpanElement.TAG);
		assert name != null : "attribute name must not be null";
		this.name = name;
		endContruct();
	}

	/**
	 * constructor by copy.
	 *
	 * @param source the source to copy
	 */
	protected DocumentMeta(DocumentMeta source) {
		super(source);
		this.name = source.name;
		this.content = source.content;
		endContruct();
	}

	/**
	 * End the construction of the widget.
	 */
	private void endContruct() {
		addAttachHandler(new PageAttachHandler());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see fr.putnami.pwt.core.editor.client.factory.CloneableWidget#cloneWidget()
	 */
	@Override
	public IsWidget cloneWidget() {
		return new DocumentMeta(this);
	}

	/**
	 * Gets the name attribute.
	 *
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the content attribute.
	 *
	 * @return the content attribute
	 */
	public String getContent() {
		return content;
	}

	/**
	 * Sets the meta content attribute.
	 *
	 * @param content the new content attribute of the meta tag
	 */
	public void setContent(String content) {
		this.content = content;
	}

	/**
	 * Return the first meta tag from the head section with name matching. <br>
	 * If createIfMissing the tag is created and added at the end of the head section.<br>
	 * <p>
	 * <strong>Note : </strong> the name is case insensitive
	 * </p>
	 *
	 * @param name the name attribute of the metta tag
	 * @param createIfMissing create the tag in the head section if missing
	 * @return meta tag element or null
	 */
	public static MetaElement getDescriptionTag(String name, boolean createIfMissing) {
		Document doc = Document.get();
		HeadElement head = doc.getHead();
		assert head != null : "No head section found in the document";
		assert name != null : "the name must not be null";

		NodeList<Element> tags = head.getElementsByTagName("meta");
		MetaElement metaTag = null;
		for (int i = 0; i < tags.getLength(); i++) {
			metaTag = (MetaElement) tags.getItem(i);
			if (name.equalsIgnoreCase(metaTag.getName())) {
				return metaTag;
			}
		}
		if (createIfMissing) {
			metaTag = doc.createMetaElement();
			metaTag.setName(name);
			head.appendChild(metaTag);
		}
		return metaTag;
	}

}
