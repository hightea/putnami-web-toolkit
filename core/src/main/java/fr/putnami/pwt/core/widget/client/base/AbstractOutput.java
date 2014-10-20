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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.ParagraphElement;
import com.google.gwt.editor.client.LeafValueEditor;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.helper.TakesValueEditorWrapper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractOutput<T> extends Widget implements EditorLeaf, EditorOutput<T>, HasResponsiveVisibility {

	private HandlerManager handlerManager;

	private Element element;

	private String tag = ParagraphElement.TAG;

	private String path;
	private T value;

	public AbstractOutput() {
	}

	protected AbstractOutput(AbstractOutput<T> source) {
		this.tag = source.tag;
		this.path = source.path;
		this.handlerManager = new HandlerManager(source.handlerManager, this);
		this.handlerManager.resetSinkEvents();
		StyleUtils.cloneStyle(this, source);
	}

	@Override
	protected HandlerManager createHandlerManager() {
		if (this.handlerManager == null) {
			this.handlerManager = new HandlerManager(this);
		}
		return this.handlerManager;
	}

	@Override
	public void sinkEvents(int eventBitsToAdd) {
		super.sinkEvents(eventBitsToAdd);
		this.createHandlerManager().sinkEvents(eventBitsToAdd);
	}

	@Override
	public void unsinkEvents(int eventBitsToRemove) {
		super.sinkEvents(eventBitsToRemove);
		this.createHandlerManager().unsinkEvents(eventBitsToRemove);
	}

	@Override
	public Element getElement() {
		return this.ensureElement();
	}

	public boolean elementExists() {
		return this.element != null;
	}

	private Element ensureElement() {
		if (this.element == null) {
			this.element = (Element) Document.get().createElement(this.tag);
			this.setElement(this.element);
			StyleUtils.initStyle(this);
			StyleUtils.addStyle(this.element, EditorOutput.STYLE_CONTROL_STATIC);
			this.ensureElement(this.element);
		}
		return this.element;
	}

	protected abstract void ensureElement(Element element);

	public void setTag(String tag) {
		this.tag = tag;
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public T getValue() {
		return this.value;
	}

	@Override
	public void edit(T value) {
		this.value = value;
		this.renderValue(value);
	}

	@Override
	public LeafValueEditor<T> asEditor() {
		return new TakesValueEditorWrapper<T>(this);
	}

	protected abstract void renderValue(T value);

	@Override
	public void setXsVisibility(Visibility xsVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.XS, xsVisibility));
	}

	@Override
	public void setSmVisibility(Visibility smVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.SM, smVisibility));
	}

	@Override
	public void setMdVisibility(Visibility mdVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.MD, mdVisibility));
	}

	@Override
	public void setLgVisibility(Visibility lgVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.LG, lgVisibility));
	}

	@Override
	public void setPrintVisibility(Visibility printVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.PRINT, printVisibility));
	}

}
