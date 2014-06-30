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
import com.google.gwt.dom.client.ParagraphElement;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.HasText;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractOutput<T> extends Widget implements
		CloneableWidget,
		EditorLeaf,
		EditorOutput<T>,
		HasText,
		HasResponsiveVisibility {

	public enum Style implements CssStyle {
		DEFAULT(null),
		LABEL("label label-default"),
		LABEL_PRIMARY("label label-primary"),
		LABEL_SUCCESS("label label-success"),
		LABEL_INFO("label label-info"),
		LABEL_WARNING("label label-warning"),
		LABEL_DANGER("label label-danger"),
		BADGE("badge");

		private final String style;

		private Style(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	private HandlerManager handlerManager;

	private Element element;

	private String tag = ParagraphElement.TAG;
	private Style style;

	private Renderer<T> renderer;

	private String path;
	private T value;

	private String text;

	public AbstractOutput() {
	}

	protected AbstractOutput(AbstractOutput<T> source) {
		tag = source.tag;
		renderer = source.renderer;
		style = source.style;
		path = source.path;
		text = source.text;
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

	@Override
	public Element getElement() {
		return ensureElement();
	}

	private Element ensureElement() {
		if (element == null) {
			element = (Element) Document.get().createElement(tag);
			setElement(element);
			if (text != null) {
				setText(text);
			}
			StyleUtils.initStyle(this);
			StyleUtils.addStyle(element, STYLE_CONTROL_STATIC);
			setStyle(style);
		}
		return element;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	@Override
	public String getPath() {
		return path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public T getValue() {
		return this.value;
	}

	public void setRenderer(Renderer<T> renderer) {
		this.renderer = renderer;
		if (renderer != null) {
			setText(renderer.render(value));
		}
	}

	@Override
	public void edit(T value) {
		this.value = value;
		setText(renderer.render(value));
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		if (element != null) {
			element.setInnerText(text);
		}
	}

	public Style getStyle() {
		return style;
	}

	public void setStyle(Style style) {
		this.style = style;
		if (element != null) {
			StyleUtils.addStyle(element, style);
		}
	}

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
