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

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public abstract class AbstractHTMLPanel extends HTMLPanel implements EditorComposite, HasResponsiveVisibility, CloneableWidget {

	protected static final String EMPTY_HTML = "";

	private final Map<Widget, String> children = Maps.newLinkedHashMap();

	private final String html;
	private final String tag;

	private HandlerManager handlerManager;

	private Set<Editor> editorChildren;
	private String path;

	public AbstractHTMLPanel(String tag, String html) {
		super(tag, html);
		this.html = html;
		this.tag = tag;
		StyleUtils.initStyle(this);
	}

	protected AbstractHTMLPanel(AbstractHTMLPanel source) {
		this(source.tag, source.html);
		this.path = source.path;
		for (Map.Entry<Widget, String> widgetEntry : source.children.entrySet()) {
			addAndReplaceElement(WidgetUtils.cloneWidget(widgetEntry.getKey()), widgetEntry.getValue());
		}
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
	public String getPath() {
		return path == null ? Path.ROOT_PATH : path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public Iterable<Editor> getEditors() {
		return editorChildren == null ? Collections.EMPTY_SET : Iterables.unmodifiableIterable(editorChildren);
	}

	@Override
	public void addAndReplaceElement(Widget widget, com.google.gwt.user.client.Element toReplace) {
		if (widget instanceof Editor) {
			if (editorChildren == null) {
				editorChildren = Sets.newLinkedHashSet();
			}
			this.editorChildren.add((Editor) widget);
		}
		String elementId = toReplace.getId();
		children.put(widget, elementId);
		toReplace.removeAttribute("id");
		super.addAndReplaceElement(widget, toReplace);
	}

	protected void insert(Widget child, int beforeIndex, boolean domInsert) {
		insert(child, getElement(), beforeIndex, domInsert);
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
