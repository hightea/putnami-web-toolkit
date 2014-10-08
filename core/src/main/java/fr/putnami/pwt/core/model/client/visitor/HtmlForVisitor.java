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
package fr.putnami.pwt.core.model.client.visitor;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import java.util.Map;
import java.util.Set;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.base.HasHtmlFor;
import fr.putnami.pwt.core.model.client.base.HasHtmlId;

public class HtmlForVisitor extends AbstractVisitor {

	private static long seq = 0;

	private final Map<Path, String> htmlForIds = Maps.newHashMap();
	private final Set<Context> htmlIdContexts = Sets.newLinkedHashSet();

	public HtmlForVisitor() {
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		B editor = context.getEditor();
		if (editor instanceof HasHtmlFor) {
			HasHtmlFor hasHtmlFor = (HasHtmlFor) editor;
			if (hasHtmlFor.getHtmlFor() == null) {
				Path path = context.getPath();
				String htmlFor = this.htmlForIds.get(path);
				if (htmlFor == null) {
					htmlFor = "uid-" + HtmlForVisitor.incrementeAndGetSeq();
					this.htmlForIds.put(path, htmlFor);
				}
				hasHtmlFor.setHtmlFor(htmlFor);
			}
		}
		if (editor instanceof HasHtmlId && !this.htmlIdContexts.contains(context)) {
			this.htmlIdContexts.add(context);
		}
		return true;
	}

	@Override
	public <A, B extends Editor> boolean afterVisit() {
		for (Context htmlIdContext : this.htmlIdContexts) {
			HasHtmlId editor = (HasHtmlId) htmlIdContext.getEditor();
			Path path = htmlIdContext.getPath();
			String htmlFor = this.htmlForIds.get(path);
			if (htmlFor != null && editor.getHtmlId() == null) {
				editor.setHtmlId(htmlFor);
			}
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

	private static long incrementeAndGetSeq() {
		return ++HtmlForVisitor.seq;
	}

}
