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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.dom.client.Document;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractWidget extends Widget implements HasResponsiveVisibility, CloneableWidget {

	private final String tagName;

	private HandlerManager handlerManager;

	private String path;

	public AbstractWidget(String tagName) {
		if (tagName != null) {
			setElement(Document.get().createElement(tagName));
		}
		this.tagName = tagName;
		StyleUtils.initStyle(this);
	}

	public AbstractWidget(AbstractWidget source) {
		this(source.tagName);
		handlerManager = new HandlerManager(source.handlerManager, this);
		handlerManager.resetSinkEvents();
		StyleUtils.cloneStyle(this, source);
	}

	@Override
	protected HandlerManager createHandlerManager() {
		if (handlerManager == null) {
			handlerManager = new HandlerManager(this);
		}
		return handlerManager;
	}

	@Override
	public void sinkEvents(int eventBitsToAdd) {
		super.sinkEvents(eventBitsToAdd);
		createHandlerManager().sinkEvents(eventBitsToAdd);
	}

	@Override
	public void unsinkEvents(int eventBitsToRemove) {
		super.sinkEvents(eventBitsToRemove);
		createHandlerManager().unsinkEvents(eventBitsToRemove);
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public void setStyleName(String style) {
		StyleUtils.addStyle(this, new SimpleStyle(style));
	}

	@Override
	public void setXsVisibility(XsVisibility xsVisibility) {
		StyleUtils.addStyle(this, xsVisibility);
	}

	@Override
	public void setSmVisibility(SmVisibility smVisibility) {
		StyleUtils.addStyle(this, smVisibility);
	}

	@Override
	public void setMdVisibility(MdVisibility mdVisibility) {
		StyleUtils.addStyle(this, mdVisibility);
	}

	@Override
	public void setLgVisibility(LgVisibility lgVisibility) {
		StyleUtils.addStyle(this, lgVisibility);
	}

	@Override
	public void setPrintVisibility(PrintVisibility printVisibility) {
		StyleUtils.addStyle(this, printVisibility);
	}

}
