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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.SpanElement;
import com.google.gwt.event.logical.shared.AttachEvent;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractWidget;

/**
 * The DocumentTitle is a widget that alter the title of the html page
 * <ul>
 * <li>On load event, the title changes.</li>
 * <li>On unload event, the title is reverted.</li>
 * </ul>
 */
public class DocumentTitle extends AbstractWidget {

	/**
	 * The PageAttachHandler handles the attach event and alter the document title.
	 */
	private class PageAttachHandler implements AttachEvent.Handler {
		private String fromTitle;

		/*
		 * (non-Javadoc)
		 *
		 * @see
		 * com.google.gwt.event.logical.shared.AttachEvent.Handler#onAttachOrDetach(com.google.gwt.event
		 * .logical.shared.AttachEvent)
		 */
		@Override
		public void onAttachOrDetach(AttachEvent event) {
			Document doc = Document.get();
			if (event.isAttached()) {
				fromTitle = doc.getTitle();
				doc.setTitle(title);
			} else {
				doc.setTitle(fromTitle);
			}
		}
	}

	private String title;

	/**
	 * Instantiates a new document title widget.
	 */
	public DocumentTitle() {
		super(SpanElement.TAG);
		endContruct();
	}

	/**
	 * constructor by copy.
	 *
	 * @param source the source to copy
	 */
	protected DocumentTitle(DocumentTitle source) {
		super(source);
		this.title = source.title;
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
		return new DocumentTitle(this);
	}

	/**
	 * Get the title of the document.
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the title of the document.
	 *
	 * @param title the new title to set on the document.
	 */
	@Override
	public void setTitle(String title) {
		this.title = title;
	}
}
