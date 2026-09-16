// DM/AE/DS以外のドメイン(CM/MH/EG等)を生成する。R版のbuild_domain_common.Rの
// build_other_domains()/build_generic_domain()/build_repeated_domain()に対応する。
// 依存関係解決の基盤(トポロジカルソート)、build_generic_domain(個別ロジックを持たないドメイン向け)、
// build_repeated_domain(TR/LB等、同一alias_name内で複数labelを持つ繰り返しドメイン向け)、
// inject_cross_domain_refs(ドメインをまたぐpresence_conditions/age_bounds解決)、
// build_other_domains本体のオーケストレーション(トポロジカルソート順の呼び分け・built_domainsへの積み上げ)、
// drug型項目(who_drug_idf、WHO Drug参照)、visit_lookup(VISIT/VISITNUM列)まで対応する。
// apply_orres_populators(LB/TR/VSのORRESを基準範囲に基づいた値に置き換える処理)は
// js/orres_realism.jsに分離して対応済み。clamp_dates_to_discontinuation(中止日より後の日付を
// 再生成する処理)、exclude_ae_linked_prefixes/merge_linked_domains(AEリンクブロックの二重生成除外・
// マージ。populate_linked_blocks/split_linked_domains本体はjs/ae_domain.jsに実装)も対応済み

// MedDRAコーディングブロック(LLT〜SOC)の列名(ae_domain.jsのMEDDRA_CODING_COLSと同じ対応表を使う)

// cdisc_variable_valuesから、cdisc_variable名 -> prefix の対応表を作る。
// MedDRAコーディングブロックの列(例: AELLTCD)はEDC仕様には存在せずaddAeMeddraCodingBlock()等で
// 独自に追加する列のため、この対応表にも明示的に加えておく(Rのbuild_cdisc_variable_to_prefix()に対応)
function buildCdiscVariableToPrefix(cdiscVariableValues) {
  const map = {};
  cdiscVariableValues.forEach((r) => {
    map[r.cdisc_variable] = r.prefix;
  });
  const prefixes = new Set(cdiscVariableValues.map((r) => r.prefix));
  prefixes.forEach((prefix) => {
    MEDDRA_CODING_COLS.forEach((col) => {
      map[prefix + col] = prefix;
    });
  });
  return map;
}

// presence_conditions/field_ref_bounds/age_boundsのうち、cdisc_variableとref_cdisc_variableのprefixが
// 異なる(=ドメインをまたぐ参照)行から、(from, to)の依存エッジ一覧を作る。fromはtoに依存する
// (toを先に生成する必要がある)(Rのbuild_cross_prefix_edges()に対応)
function buildCrossPrefixEdges(presenceConditions, fieldRefBounds, cdiscVariableToPrefix, ageBounds, dateRefBounds) {
  const pairs = [
    ...(presenceConditions || []).map((r) => [r.cdisc_variable, r.ref_cdisc_variable]),
    // age_gt/age_ge/age_lt/age_le型のpresence_conditionsはref2_cdisc_variable(もう一方の参照先)も持つ
    ...(presenceConditions || []).map((r) => [r.cdisc_variable, r.ref2_cdisc_variable]),
    ...(fieldRefBounds || []).map((r) => [r.cdisc_variable, r.ref_cdisc_variable]),
    ...(ageBounds || []).map((r) => [r.cdisc_variable, r.ref_cdisc_variable]),
    ...(dateRefBounds || []).map((r) => [r.cdisc_variable, r.ref_cdisc_variable]),
  ];
  const seen = new Set();
  const result = [];
  pairs.forEach(([cdiscVariable, refCdiscVariable]) => {
    const prefix = cdiscVariableToPrefix[cdiscVariable];
    const refPrefix = cdiscVariableToPrefix[refCdiscVariable];
    if (!prefix || !refPrefix || prefix === refPrefix) return;
    const key = `${prefix}|${refPrefix}`;
    if (seen.has(key)) return;
    seen.add(key);
    result.push({ from: prefix, to: refPrefix });
  });
  return result;
}

// prefixesを、edges(from依存toの依存関係)に基づいて依存先が先に来るように並べ替える(トポロジカルソート)。
// 循環参照がある場合は、それ以上並べ替えできない分をそのまま残りの順序で追加する(Rのtopo_sort_prefixes()に対応)
function topoSortPrefixes(prefixes, edges) {
  const scopedEdges = edges.filter((e) => prefixes.includes(e.from) && prefixes.includes(e.to));
  let remaining = [...prefixes];
  const ordered = [];
  while (remaining.length > 0) {
    const remainingSet = new Set(remaining);
    const ready = remaining.filter((p) => !scopedEdges.some((e) => e.from === p && remainingSet.has(e.to)));
    if (ready.length === 0) {
      ordered.push(...remaining);
      break;
    }
    ordered.push(...ready);
    remaining = remaining.filter((p) => !ready.includes(p));
  }
  return ordered;
}

// topoSortPrefixes()と同じアルゴリズムだが、循環(依存が解決できず「ready」が空になる時点)に
// 達したら、そこで打ち切ってordered(そこまでに確定した順序)とremaining(未解決のまま残った、
// 循環に関与する・またはそれに依存しているprefix集合)を分けて返す。buildOtherDomains()が、
// remainingの部分だけをシート(alias_name)単位で改めて依存解決するために使う(Rの
// topo_sort_prefixes_with_leftover()に対応)
function topoSortPrefixesWithLeftover(prefixes, edges) {
  const scopedEdges = edges.filter((e) => prefixes.includes(e.from) && prefixes.includes(e.to));
  let remaining = [...prefixes];
  const ordered = [];
  while (remaining.length > 0) {
    const remainingSet = new Set(remaining);
    const ready = remaining.filter((p) => !scopedEdges.some((e) => e.from === p && remainingSet.has(e.to)));
    if (ready.length === 0) {
      break;
    }
    ordered.push(...ready);
    remaining = remaining.filter((p) => !ready.includes(p));
  }
  return { ordered, remaining };
}

// presenceConditions/fieldRefBounds/ageBounds/dateRefBoundsから、(prefix, aliasName)をノードとする
// 依存エッジ一覧を作る(buildCrossPrefixEdges()のシート単位版)。prefix単位のbuildCrossPrefixEdges()と
// 異なり、同じprefix内の別aliasNameへの参照も含める(Rのbuild_alias_level_edges()に対応)
function buildAliasLevelEdges(presenceConditions, fieldRefBounds, cdiscVariableToPrefix, ageBounds, dateRefBounds) {
  const rows = [
    ...(presenceConditions || []).map((r) => ({ aliasName: r.alias_name, cdiscVariable: r.cdisc_variable, refCdiscVariable: r.ref_cdisc_variable, refAliasName: r.ref_alias_name })),
    // age_gt/age_ge/age_lt/age_le型のpresence_conditionsはref2_cdisc_variable/ref2_alias_name(もう一方の
    // 参照先)も持つ
    ...(presenceConditions || []).map((r) => ({ aliasName: r.alias_name, cdiscVariable: r.cdisc_variable, refCdiscVariable: r.ref2_cdisc_variable, refAliasName: r.ref2_alias_name })),
    // fieldRefBounds(formula参照)は必ず同一シート内の参照のため、refAliasNameは自分自身と同じ
    ...(fieldRefBounds || []).map((r) => ({ aliasName: r.alias_name, cdiscVariable: r.cdisc_variable, refCdiscVariable: r.ref_cdisc_variable, refAliasName: r.alias_name })),
    ...(ageBounds || []).map((r) => ({ aliasName: r.alias_name, cdiscVariable: r.cdisc_variable, refCdiscVariable: r.ref_cdisc_variable, refAliasName: r.ref_alias_name })),
    ...(dateRefBounds || []).map((r) => ({ aliasName: r.alias_name, cdiscVariable: r.cdisc_variable, refCdiscVariable: r.ref_cdisc_variable, refAliasName: r.ref_alias_name })),
  ];
  const seen = new Set();
  const result = [];
  rows.forEach(({ aliasName, cdiscVariable, refCdiscVariable, refAliasName }) => {
    if (aliasName == null || refAliasName == null) return;
    const fromPrefix = cdiscVariableToPrefix[cdiscVariable];
    const toPrefix = cdiscVariableToPrefix[refCdiscVariable];
    if (!fromPrefix || !toPrefix) return;
    const key = `${fromPrefix}|${aliasName}|${toPrefix}|${refAliasName}`;
    if (seen.has(key)) return;
    seen.add(key);
    result.push({ fromPrefix, fromAlias: aliasName, toPrefix, toAlias: refAliasName });
  });
  return result;
}

// nodes({prefix, aliasName}の配列)を、edges({fromPrefix, fromAlias, toPrefix, toAlias}。fromはtoに依存する)
// に基づいてトポロジカルソートする。topoSortPrefixes()の(prefix, aliasName)複合キー版
// (Rのtopo_sort_prefix_aliases()に対応)
function topoSortPrefixAliases(nodes, edges) {
  const nodeKey = (prefix, alias) => `${prefix}${alias}`;
  const seenNodes = new Set();
  const uniqueNodes = [];
  nodes.forEach((n) => {
    const key = nodeKey(n.prefix, n.alias_name);
    if (seenNodes.has(key)) return;
    seenNodes.add(key);
    uniqueNodes.push({ prefix: n.prefix, alias_name: n.alias_name, key });
  });
  const scopedEdges = edges
    .map((e) => ({ fromKey: nodeKey(e.fromPrefix, e.fromAlias), toKey: nodeKey(e.toPrefix, e.toAlias) }))
    .filter((e) => seenNodes.has(e.fromKey) && seenNodes.has(e.toKey) && e.fromKey !== e.toKey);

  let remaining = uniqueNodes.map((n) => n.key);
  const orderedKeys = [];
  while (remaining.length > 0) {
    const remainingSet = new Set(remaining);
    const ready = remaining.filter((k) => !scopedEdges.some((e) => e.fromKey === k && remainingSet.has(e.toKey)));
    if (ready.length === 0) {
      orderedKeys.push(...remaining);
      break;
    }
    orderedKeys.push(...ready);
    remaining = remaining.filter((k) => !ready.includes(k));
  }
  const byKey = new Map(uniqueNodes.map((n) => [n.key, n]));
  return orderedKeys.map((k) => byKey.get(k));
}

// buildSubjectActiveSheets()が返すactiveSheets({USUBJID: Set(alias_name)})を、
// (USUBJID, alias_name)の縦持り配列に変換する(Rのactive_sheet_membership_table()に対応)
function buildActiveSheetTable(activeSheets) {
  const rows = [];
  Object.keys(activeSheets).forEach((usubjid) => {
    activeSheets[usubjid].forEach((aliasName) => {
      rows.push({ USUBJID: usubjid, alias_name: aliasName });
    });
  });
  return rows;
}

// edcSpec.sheetsのうちcategory=="visit"のシートは、name(シート表示名)の末尾に"(VisitName)"という形で
// edcSpec.visitsのnameが含まれている(例: "効果判定報告(End of Induction Cycle1)")。これを抽出して
// visitsと突き合わせ、(alias_name, VISIT, VISITNUM)の配列を作る。一致しない行(末尾が"(...)"形式で
// ない等)は含めない(Rのbuild_visit_lookup()に対応)
function buildVisitLookup(sheets, visits) {
  if (!visits || visits.length === 0) return [];
  const visitNumByName = {};
  visits.forEach((v) => {
    visitNumByName[v.name] = v.num;
  });
  const result = [];
  (sheets || []).forEach((sheet) => {
    if (sheet.category !== "visit") return;
    const m = /\(([^()]+)\)$/.exec(sheet.name || "");
    if (!m) return;
    const visitName = m[1];
    if (!(visitName in visitNumByName)) return;
    result.push({ alias_name: sheet.alias_name, VISIT: visitName, VISITNUM: visitNumByName[visitName] });
  });
  return result;
}

// visitLookup(alias_name, VISIT, VISITNUM)をalias_nameで結合し、VISIT/VISITNUM列を追加する。
// category=="visit"のシート由来でない行(一致しない行)はnullのまま。このドメインにvisitカテゴリの
// シート由来の行が1件も無ければ(全行null)、意味の無い空列を出さないよう列自体を追加しない。
// また、既にVISITNUM列がある(そのドメイン自身がVISITNUMをradio_button等で直接定義している)場合は
// 上書きしない(Rのadd_visit_columns()に対応)
function addVisitColumns(data, visitLookup) {
  if (!visitLookup || visitLookup.length === 0 || !data[0] || !("alias_name" in data[0]) || "VISITNUM" in data[0]) {
    return data;
  }
  const lookupByAlias = {};
  visitLookup.forEach((r) => {
    if (!(r.alias_name in lookupByAlias)) lookupByAlias[r.alias_name] = r;
  });
  const anyMatch = data.some((row) => row.alias_name in lookupByAlias);
  if (!anyMatch) return data;
  data.forEach((row) => {
    const hit = lookupByAlias[row.alias_name];
    row.VISIT = hit ? hit.VISIT : null;
    row.VISITNUM = hit ? hit.VISITNUM : null;
  });
  return data;
}

// candidates([{USUBJID, alias_name}, ...]、USUBJIDごとに複数のalias_name候補がある)の各USUBJIDについて、
// 1つのalias_nameを選ぶ。presenceConditions(このドメイン自身のcdisc_variableに絞り込み済み)から、
// 候補のalias_nameが実際にゲーティング条件を満たす(=値が入る)ものであれば、それを優先して選ぶ。
// 条件を満たす候補が無い、builtDomainsに参照先ドメインがまだ無い、またはpresenceConditionsに
// 該当するequals条件が無い場合はランダムに1つ選ぶ(Rのresolve_preferred_alias_name()に対応)
function resolvePreferredAliasName(candidates, presenceConditions, builtDomains, cdiscVariableToPrefix) {
  const groupByUsubjid = (list) => {
    const byUsubjid = {};
    list.forEach((c) => {
      if (!byUsubjid[c.USUBJID]) byUsubjid[c.USUBJID] = [];
      byUsubjid[c.USUBJID].push(c);
    });
    return byUsubjid;
  };
  const randomOnePerUsubjid = (list) => {
    const byUsubjid = groupByUsubjid(list);
    return Object.keys(byUsubjid).map((usubjid) => sampleOne(byUsubjid[usubjid]));
  };

  const equalsConditions = (presenceConditions || []).filter((pc) => pc.condition_type === "equals" && pc.ref_alias_name != null);
  if (equalsConditions.length === 0) {
    return randomOnePerUsubjid(candidates);
  }

  const groups = new Map();
  equalsConditions.forEach((pc) => {
    const key = `${pc.ref_cdisc_variable}|${pc.ref_alias_name}|${pc.ref_label}`;
    if (!groups.has(key)) {
      groups.set(key, { refCdiscVariable: pc.ref_cdisc_variable, refAliasName: pc.ref_alias_name, refLabel: pc.ref_label, expectedValues: new Set() });
    }
    groups.get(key).expectedValues.add(pc.expected_value);
  });

  const satisfied = new Set();
  groups.forEach((g) => {
    const refPrefix = cdiscVariableToPrefix[g.refCdiscVariable];
    if (!refPrefix || !builtDomains || !builtDomains[refPrefix]) return;
    const refData = builtDomains[refPrefix];
    const hasAliasLabel = refData[0] && "alias_name" in refData[0] && "label" in refData[0];
    const refSlice =
      hasAliasLabel && g.refLabel != null
        ? refData.filter((r) => r.alias_name === g.refAliasName && r.label === g.refLabel)
        : refData;
    refSlice.forEach((r) => {
      if (g.expectedValues.has(r[g.refCdiscVariable])) {
        satisfied.add(`${r.USUBJID}|${g.refAliasName}`);
      }
    });
  });

  if (satisfied.size === 0) {
    return randomOnePerUsubjid(candidates);
  }

  const byUsubjid = groupByUsubjid(candidates);
  return Object.keys(byUsubjid).map((usubjid) => {
    const list = byUsubjid[usubjid];
    const satisfiedList = list.filter((c) => satisfied.has(`${c.USUBJID}|${c.alias_name}`));
    const pool = satisfiedList.length > 0 ? satisfiedList : list;
    return sampleOne(pool);
  });
}

// presence_conditions/field_ref_bounds/age_boundsが参照するcdisc_variableのうち、dataにまだ無いものを、
// 既に生成済みのbuiltDomainsから探して結合する(他ドメイン参照)。
// refAliasName/refLabel(参照先フィールド自身が属する固定のブロック、例: RSがSCの特定labelを参照する場合)が
// 分かっていればそのインスタンスに固定して結合する(USUBJIDのみ)。
// 無指定の場合は、両者がalias_name/labelを持てばそれも突き合わせキーにする(同じブロック内の参照)。
// どちらの情報も無ければUSUBJIDのみで結合する(参照元に複数レコードあると最初の1件を使う)。
// 戻り値は{ data, injectedCols }(injectedColsはこのために追加した列名。呼び出し側でゲーティングに
// 使い終わった後に削除する想定)(Rのinject_cross_domain_refs()に対応)
function injectCrossDomainRefs(data, presenceConditions, fieldRefBounds, builtDomains, cdiscVariableToPrefix, ageBounds, dateRefBounds, ownPrefix) {
  if (!data || data.length === 0) return { data, injectedCols: [] };

  // label(own_label)は、この参照条件が定義されている側(dataになる予定のドメイン自身)のインスタンス。
  // 同じref_cdisc_variable(例: RSORRES)でも、参照元のlabelブロックごとに参照先のref_labelが
  // 異なる場合(例: MHの5つのSPDEVIDブロックが、それぞれ別のRSブロック(034/035/036/...)を参照する)、
  // これを保持しておかないと、後段でどのpinをdataのどの行に適用すべきか判定できない
  // (fieldRefBoundsはlabelを持たないため、その場合はnullのままになる)。
  // dateRefBoundsのref_alias_nameは、ref('sheet_alias', N)形式の他シート参照があればその参照先
  // シート、無ければ自分自身と同じalias_name(build_generation_constraints.jsのdate_ref_bounds
  // 構築時に補われている)
  const refInstances = [
    ...(presenceConditions || []).map((r) => ({ ownAlias: r.alias_name, label: r.label, ref_cdisc_variable: r.ref_cdisc_variable, ref_alias_name: r.ref_alias_name, ref_label: r.ref_label })),
    // age_gt/age_ge/age_lt/age_le型のpresence_conditionsはref2_cdisc_variable(もう一方の参照先)も持つ
    ...(presenceConditions || []).map((r) => ({ ownAlias: r.alias_name, label: r.label, ref_cdisc_variable: r.ref2_cdisc_variable, ref_alias_name: r.ref2_alias_name, ref_label: r.ref2_label })),
    ...(fieldRefBounds || []).map((r) => ({ ownAlias: null, label: null, ref_cdisc_variable: r.ref_cdisc_variable, ref_alias_name: null, ref_label: null })),
    ...(ageBounds || []).map((r) => ({ ownAlias: r.alias_name, label: r.label, ref_cdisc_variable: r.ref_cdisc_variable, ref_alias_name: r.ref_alias_name, ref_label: r.ref_label })),
    ...(dateRefBounds || []).map((r) => ({ ownAlias: r.alias_name, label: r.label, ref_cdisc_variable: r.ref_cdisc_variable, ref_alias_name: r.ref_alias_name, ref_label: r.ref_label })),
  ].filter((r) => r.ref_cdisc_variable != null);

  const hasDataAliasName = "alias_name" in data[0];
  const hasDataAlias = hasDataAliasName && "label" in data[0];
  const dataAliasNames = hasDataAliasName ? new Set(data.map((r) => r.alias_name)) : new Set();

  const injectedCols = [];
  const refVars = [...new Set(refInstances.map((r) => r.ref_cdisc_variable))];

  refVars.forEach((refVar) => {
    if (data.some((row) => refVar in row)) return;
    const refPrefix = cdiscVariableToPrefix ? cdiscVariableToPrefix[refVar] : null;
    if (!refPrefix || !builtDomains || !builtDomains[refPrefix] || builtDomains[refPrefix].length === 0) return;
    // ownPrefixが指定されている場合、参照先が自分自身のドメインなら注入しない。wave分割時、
    // builtDomains[ownPrefix]には前waveまでの未finalizeな結果が既に入っているため、素通りさせると
    // data自身に同名列が「既にある」ことになり、このあとの通常の値生成(populateDateFields等)が
    // その変数をまるごとスキップしてしまう(このケースはresolveDateRefBoundVals()のexistingDataフォールバックで
    // 別途正しく処理される)
    if (ownPrefix && refPrefix === ownPrefix) return;
    const refData = builtDomains[refPrefix];
    // refData[0](先頭行)だけで列の有無を判定すると、wave分割で蓄積されたデータは行ごとに
    // 保持する列が異なりうる(例: SVは複数aliasのwaveに分けて生成されるため、先頭行がたまたま
    // SVENDTCを持たないalias(prephase)の行で、後方にSVENDTCを持つalias(maitenance)の行が
    // 存在していても「列が無い」と誤判定されていた)。全行を見て判定する
    if (!refData.some((row) => refVar in row)) return;
    const hasRefAlias = "alias_name" in refData[0] && "label" in refData[0];
    // 参照先がbuildGenericDomain由来(例: SV)の場合、alias_nameはあってもlabelが無い
    // (繰り返し項目を持たないため)。hasRefAliasはlabelも必須なのでこのケースではfalseになるが、
    // alias_name自体は参照先の絞り込みに使えるので別途保持しておく
    const hasRefAliasOnly = "alias_name" in refData[0];

    const resultCol = new Array(data.length).fill(null);

    const pinKeys = new Set();
    const pins = [];
    refInstances
      .filter((r) => r.ref_cdisc_variable === refVar)
      .forEach((r) => {
        const key = `${r.ownAlias}|${r.label}|${r.ref_alias_name}|${r.ref_label}`;
        if (!pinKeys.has(key)) {
          pinKeys.add(key);
          pins.push({ ownAlias: r.ownAlias, ownLabel: r.label, alias: r.ref_alias_name, label: r.ref_label });
        }
      });

    pins.forEach((pin) => {
      const pinAlias = pin.alias;
      const pinLabel = pin.label;
      const ownAlias = pin.ownAlias;
      const ownLabel = pin.ownLabel;

      // 絞り込みはown_alias/own_label(この条件が定義されているdata自身のインスタンス)を最優先する。
      // own_aliasが分かっている(=presence/age/date_ref_boundsのようにown_aliasを持つ)pinは、
      // own_aliasがこのwave/呼び出しのdataに存在しない場合、その行はそもそもこのdataに存在しない
      // (wave分割で別waveに分かれている)ので対象0件とする。ここでdata.map(() => true)のような
      // 「全行対象」にフォールバックしてしまうと、このdataに含まれる別のalias(例: erwasp)向けの
      // 値を、無関係な他alias向けのpin(例: prephase向け)が後から上書きしてしまう(実際に発生した
      // バグ: wave分割によりdateRefBoundsが同じprefix内の全alias分を含むようになり、
      // own_alias/pin_aliasのどちらも今回のdataに無いpinが多数生じ、最後に処理されたpinの値が
      // 無関係なaliasの行にまで書き込まれていた)。own_aliasが無い場合(fieldRefBounds由来、真に
      // 外部の固定参照)のみ、従来通りpin_alias/pin_labelで判定するか、それも無ければ全行を対象にする。
      // own_labelが無いとpin_labelがたまたまdata自身のlabelの1つと一致するかでしか判定できず、
      // 参照元と参照先のlabelの語彙が違う(例: MHのlabelは047〜051、RSのlabelは034/035/036/...)場合に
      // 絞り込みが常に失敗し、最後に処理したpinの値が全ブロックに上書きされてしまう(既知のバグ)
      let targetRows;
      if (ownAlias != null) {
        if (hasDataAliasName && dataAliasNames.has(ownAlias)) {
          targetRows = data.map((row) => row.alias_name === ownAlias);
          if (hasDataAlias && ownLabel != null) {
            targetRows = data.map((row, i) => targetRows[i] && row.label === ownLabel);
          }
        } else {
          targetRows = data.map(() => false);
        }
      } else if (hasDataAliasName && pinAlias != null && dataAliasNames.has(pinAlias)) {
        targetRows = data.map((row) => row.alias_name === pinAlias);
        if (hasDataAlias && pinLabel != null) {
          const labelsInAlias = new Set(data.filter((row, i) => targetRows[i]).map((row) => row.label));
          if (labelsInAlias.has(pinLabel)) {
            targetRows = data.map((row, i) => targetRows[i] && row.label === pinLabel);
          }
        }
      } else {
        targetRows = data.map(() => true);
      }
      if (!targetRows.some((v) => v)) return;

      if (pinLabel != null && hasRefAlias) {
        const valueMap = {};
        refData.forEach((row) => {
          if (row.alias_name === pinAlias && row.label === pinLabel && !(row.USUBJID in valueMap)) {
            valueMap[row.USUBJID] = row[refVar];
          }
        });
        data.forEach((row, i) => {
          if (targetRows[i]) resultCol[i] = row.USUBJID in valueMap ? valueMap[row.USUBJID] : null;
        });
      } else if (pinAlias != null && hasRefAliasOnly) {
        // 参照先にlabelが無い(buildGenericDomain由来、例: SV)場合、alias_nameだけで絞り込む。
        // ここで絞り込まずUSUBJIDだけで結合すると、参照先ドメインの中で最初に出現したalias
        // (実際に参照したいaliasとは無関係な、ビルド順が早いだけの別シート)の値を拾ってしまう
        // (実際に発生したバグ: SVはalias_nameはあるがlabelを持たないため、従来はこの絞り込みが
        // 一切効かず、常に別シートの値が誤って注入されていた)
        const valueMap = {};
        refData.forEach((row) => {
          if (row.alias_name === pinAlias && !(row.USUBJID in valueMap)) {
            valueMap[row.USUBJID] = row[refVar];
          }
        });
        data.forEach((row, i) => {
          if (targetRows[i]) resultCol[i] = row.USUBJID in valueMap ? valueMap[row.USUBJID] : null;
        });
      } else if (hasDataAlias && hasRefAlias) {
        const refMap = {};
        refData.forEach((row) => {
          const key = `${row.USUBJID}|${row.alias_name}|${row.label}`;
          if (!(key in refMap)) refMap[key] = row[refVar];
        });
        data.forEach((row, i) => {
          if (targetRows[i]) {
            const key = `${row.USUBJID}|${row.alias_name}|${row.label}`;
            resultCol[i] = key in refMap ? refMap[key] : null;
          }
        });
      } else {
        const valueMap = {};
        refData.forEach((row) => {
          if (!(row.USUBJID in valueMap)) valueMap[row.USUBJID] = row[refVar];
        });
        data.forEach((row, i) => {
          if (targetRows[i]) resultCol[i] = row.USUBJID in valueMap ? valueMap[row.USUBJID] : null;
        });
      }
    });

    data.forEach((row, i) => {
      row[refVar] = resultCol[i];
    });
    injectedCols.push(refVar);
  });

  return { data, injectedCols };
}

// USUBJID×alias_nameでグループ化し、multiRecordAliasNamesに該当するalias_nameの行だけ、
// そのグループ内の連番("alias_name"+通番)にSPIDを振り直す(Rのapply_multi_record_spid()に対応)
function applyMultiRecordSpid(data, spidVar, multiRecordAliasNames) {
  if (!multiRecordAliasNames || multiRecordAliasNames.length === 0 || !data[0] || !("alias_name" in data[0])) {
    return data;
  }
  const set = new Set(multiRecordAliasNames);
  const counters = {};
  data.forEach((row) => {
    if (!set.has(row.alias_name)) return;
    const key = `${row.USUBJID}|${row.alias_name}`;
    counters[key] = (counters[key] || 0) + 1;
    row[spidVar] = `${row.alias_name}${counters[key]}`;
  });
  return data;
}

// radio_button/check_box型の項目に、選択肢(code、無ければdefault_value)からランダムな値を入れる。
// alias_name(どのブロックの行か)によって同じcdisc_variableでも選択肢が異なりうるため、
// 行のalias_nameと一致するspecの選択肢だけから選ぶ(Rのpopulate_radio_button_fields()の
// has_alias_name==TRUEの分岐に対応。isRequired/numericBoundsの扱いはDM/AE/DSと同じ)
function populateGenericChoiceFields(data, spec, numericBounds) {
  const existingColumns = new Set(Object.keys(data[0] || {}));
  const choiceSpec = spec.filter((r) => r.field_type === "radio_button" || r.field_type === "check_box");
  const targetVars = [...new Set(choiceSpec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v));

  targetVars.forEach((varName) => {
    data.forEach((row) => {
      row[varName] = null;
    });
    const varRows = choiceSpec.filter((r) => r.cdisc_variable === varName);
    const aliasNamesForVar = [...new Set(varRows.map((r) => r.alias_name))];
    aliasNamesForVar.forEach((an) => {
      const anRows = varRows.filter((r) => r.alias_name === an);
      let choices = [...new Set(anRows.map((r) => (r.code != null ? r.code : r.default_value)))];
      const isVisible = !anRows.some((r) => r.is_invisible);
      const isRequired = anRows.some((r) => r.is_required);
      if (!isRequired && isVisible) {
        choices = [...new Set([...choices, ""])];
      }
      const bounds = numericBounds && numericBounds[varName];
      if (bounds) {
        choices = choices.filter((c) => {
          const n = Number(c);
          if (Number.isNaN(n)) return true;
          if (bounds.min_value != null && n < bounds.min_value) return false;
          if (bounds.max_value != null && n > bounds.max_value) return false;
          return true;
        });
      }
      const isCheckBox = anRows.some((r) => r.field_type === "check_box");
      const targetRows = data.filter((row) => row.alias_name === an);
      if (choices.length === 0 || targetRows.length === 0) return;
      if (isCheckBox) {
        const values = sampleCheckBoxValuesWithCoverage(choices, targetRows.length);
        targetRows.forEach((row, i) => {
          row[varName] = values[i];
        });
      } else {
        const values = sampleValuesWithCoverage(choices, targetRows.length);
        targetRows.forEach((row, i) => {
          row[varName] = values[i];
        });
      }
    });
  });
  return data;
}

// date型の項目に、registrationStartDate〜今日の間のランダムな日付を入れる。alias_nameによって
// 同じcdisc_variableでも定義の有無が異なりうるため、その変数を定義しているalias_nameの行だけに
// 値を入れる(Rのpopulate_date_fields()のhas_alias_name==TRUEの分岐に対応)。
// dateRefBoundsが渡された場合、validate_date_after_or_equal_to/validate_date_before_or_equal_to
// (他フィールド参照)による下限/上限(参照先フィールドの値、同じ行)を一律の範囲より優先する
// dateRefBounds(cdisc_variable, alias_name, label, ref_cdisc_variable, ref_alias_name, ref_label,
// bound_type)のうちvarName/boundTypeValに該当する行から、dataの各行に対応する参照先の値の配列を返す
// (該当行が無ければnull)。
// 通常のケース(参照先が別のcdisc_variable。同じ行(同じUSUBJID・同じブロック)から直接参照できる、
// 例: ECENDTC>=ECSTDTC)は、単純にrow[refVar]を使う。
// 参照先がvarName自身(同じcdisc_variable名を、別のalias_nameが参照している。例: inductionのSVSTDTCが
// prephaseのSVSTDTCを参照する、ref('prephase', 825)のようなケース)の場合は、単純な同じ行からの参照が
// できない(自分自身を参照することになってしまう)ため、USUBJID単位で参照先ブロック(refAlias/refLabel)の
// 値を引く。
// 同じvarNameに対して複数の行(alias_nameごとに異なる参照先を持つ)がある場合は、それぞれ自分の
// alias_nameの行だけに適用し、boundTypeValに応じて(min_dateは大きい方、max_dateは小さい方を)組み合わせる
// (Rのresolve_date_ref_bound_vals()に対応)
function resolveDateRefBoundVals(data, dateRefBounds, varName, boundTypeVal, existingData) {
  const hasAliasName = !!data[0] && "alias_name" in data[0];
  const hasLabel = hasAliasName && "label" in data[0];
  const hasExisting = !!existingData && existingData.length > 0 && "alias_name" in existingData[0];
  const existingHasLabel = hasExisting && "label" in existingData[0];
  const boundRows = (dateRefBounds || []).filter(
    (r) => r.cdisc_variable === varName && r.bound_type === boundTypeVal && data[0] && r.ref_cdisc_variable in data[0]
  );
  if (boundRows.length === 0) return null;

  const result = new Array(data.length).fill(null);
  boundRows.forEach((br) => {
    const refVar = br.ref_cdisc_variable;
    // ref('sheet_alias', N)+N.days/-N.daysの符号付き日数オフセット(extractDateCrossRefOffsetDays()由来。
    // 無指定ならnull=0として扱う)
    const offsetDays = br.offset_days != null ? br.offset_days : 0;
    const ownAlias = br.alias_name != null ? br.alias_name : null;
    const ownLabel = br.label != null ? br.label : null;
    const refAlias = br.ref_alias_name != null ? br.ref_alias_name : null;
    const refLabel = br.ref_label != null ? br.ref_label : null;
    // 同じcdisc_variable名を参照する自己参照(refVar===varName)には、alias_nameが異なる場合(例: induction
    // のSVSTDTCがprephaseのSVSTDTCを参照)だけでなく、同一alias内でlabelだけが異なる場合(例: BLASTLE(005)
    // がWBC(006)のLBDTCを参照)も含める。後者を素通りさせて下のelse節(values = row[refVar]、つまり
    // 自分自身の現在値)に落ちると、常に「自分自身と等しい」という無意味な比較になり、参照先(WBC)が
    // 後続のclampで動いても追従できなくなる(実際に発生したバグ: WBC/BLASTLEの等号制約が崩れた)
    const isSelfRefAcrossAliasOrLabel =
      refVar === varName &&
      hasAliasName &&
      ownAlias != null &&
      refAlias != null &&
      (ownAlias !== refAlias || (hasLabel && ownLabel != null && refLabel != null && ownLabel !== refLabel));

    let values;
    if (isSelfRefAcrossAliasOrLabel) {
      const refMap = {};
      data.forEach((row) => {
        if (row.alias_name === refAlias && (refLabel == null || !hasLabel || row.label === refLabel)) {
          if (!(row.USUBJID in refMap)) refMap[row.USUBJID] = row[refVar];
        }
      });
      // このdata(1つのbuild呼び出し=1 wave分)に参照先aliasの行が無い場合、既にドメイン単位/シート単位の
      // 依存順序解決(wave分割)で先に確定済みのexistingData(前waveの結果)から探す。これにより、
      // 「ドメイン(prefix)単位では循環に見えるが、実際にはシート単位では循環でない」参照チェーン
      // (例: SV(hr3fisrt)→RS→LB→FA→SV(prephase))を正しく解決できる
      if (Object.keys(refMap).length === 0 && hasExisting && refVar in existingData[0]) {
        existingData.forEach((row) => {
          if (row.alias_name === refAlias && (refLabel == null || !existingHasLabel || row.label === refLabel)) {
            if (!(row.USUBJID in refMap)) refMap[row.USUBJID] = row[refVar];
          }
        });
      }
      values = data.map((row) => (row.USUBJID in refMap ? refMap[row.USUBJID] : null));
    } else {
      values = data.map((row) => row[refVar]);
    }
    if (offsetDays !== 0) {
      values = values.map((v) => (v != null ? addDaysToDateString(v, offsetDays) : v));
    }

    data.forEach((row, i) => {
      if (hasAliasName && ownAlias != null && row.alias_name !== ownAlias) return;
      // labelがある場合、この制約はown_label(br.label)の行にだけ適用すべき。フィルタしないと、
      // 同じaliasの他label(例: WBC自身の行)にまで「BLASTLE用の下限」が誤って適用されてしまう
      if (hasLabel && ownLabel != null && row.label !== ownLabel) return;
      const v = values[i];
      if (v == null) return;
      if (result[i] == null) {
        result[i] = v;
      } else if (boundTypeVal === "min_date") {
        if (v > result[i]) result[i] = v;
      } else if (v < result[i]) {
        result[i] = v;
      }
    });
  });
  return result.every((v) => v == null) ? null : result;
}

// builtDomains.DM(既に生成済みのDM)のRFSTDTCをUSUBJID単位で結合する。既にRFSTDTC列がある場合
// (自分自身がDMの場合など)やDMがまだ無い場合は何もしない。明示的なref()参照を持たない日付項目でも、
// 下の日付生成でのデフォルト下限(registrationStartDate、試験共通の定数)ではなく被験者本人の
// 登録日を下限にできるようにするため(戻り値のinjectedで、呼び出し側が最終出力から取り除く列に
// 加えられるようにする。Rのinject_dm_rfstdtc()に対応)
function injectDmRfstdtc(data, builtDomains) {
  if (data[0] && "RFSTDTC" in data[0]) {
    return { data, injected: false };
  }
  const dm = (builtDomains || {}).DM;
  if (!dm || !dm[0] || !("RFSTDTC" in dm[0])) {
    return { data, injected: false };
  }
  const rfstdtcByUsubjid = {};
  dm.forEach((row) => {
    if (!(row.USUBJID in rfstdtcByUsubjid)) rfstdtcByUsubjid[row.USUBJID] = row.RFSTDTC;
  });
  data.forEach((row) => {
    row.RFSTDTC = row.USUBJID in rfstdtcByUsubjid ? rfstdtcByUsubjid[row.USUBJID] : null;
  });
  return { data, injected: true };
}

function populateGenericDateFields(data, spec, registrationStartDate, dateRefBounds, existingData) {
  const existingColumns = new Set(Object.keys(data[0] || {}));
  let dateVars = [...new Set(spec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))].filter(
    (v) => !existingColumns.has(v)
  );
  dateVars = sortDateVarsByDependency(dateVars, dateRefBounds);
  const today = new Date().toISOString().slice(0, 10);

  dateVars.forEach((varName) => {
    const dateAliasNames = new Set(spec.filter((r) => r.field_type === "date" && r.cdisc_variable === varName).map((r) => r.alias_name));
    const minRefVals = resolveDateRefBoundVals(data, dateRefBounds, varName, "min_date", existingData);
    const maxRefVals = resolveDateRefBoundVals(data, dateRefBounds, varName, "max_date", existingData);
    data.forEach((row, i) => {
      if (!dateAliasNames.has(row.alias_name)) {
        row[varName] = null;
        return;
      }
      let lower = registrationStartDate;
      // RFSTDTC(症例登録日)は、この変数に明示的なmin_date参照(minRefVals)が無い場合の
      // デフォルト下限としてのみ使う。明示的な参照がある変数にまで一律にRFSTDTCを下限に加えると、
      // その変数本来の(RFSTDTCより前を許容する)意味を壊してしまうため
      const hasMinRef = minRefVals != null && minRefVals[i] != null;
      if (!hasMinRef && row.RFSTDTC != null && row.RFSTDTC > lower) lower = row.RFSTDTC;
      if (hasMinRef) {
        const refVal = minRefVals[i];
        if (new Date(refVal).getTime() > new Date(lower).getTime()) lower = refVal;
      }
      let upper = today;
      if (maxRefVals != null && maxRefVals[i] != null) {
        const refVal = maxRefVals[i];
        if (new Date(refVal).getTime() < new Date(upper).getTime()) upper = refVal;
      }
      if (new Date(upper).getTime() < new Date(lower).getTime()) upper = lower;
      row[varName] = randomDateBetween(lower, upper);
    });
  });
  return data;
}

// 変数名が"DOSE"で終わる項目に、それらしい用量の数値を入れる(Rのpopulate_dose_fields()に対応)
function populateDoseFields(data, spec) {
  const existingColumns = new Set(Object.keys(data[0] || {}));
  const doseVars = [...new Set(spec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v) && /DOSE$/.test(v));
  const doseChoices = ["50", "100", "150", "200", "250", "300", "400", "500"];
  doseVars.forEach((varName) => {
    data.forEach((row) => {
      row[varName] = sampleOne(doseChoices);
    });
  });
  return data;
}

// radio_button/check_box/date/doseのいずれでも埋まらなかった対象変数に、とりあえずDUMMY値を格納する
// (Rのpopulate_dummy_fields()に対応)
function populateGenericDummyFields(data, spec) {
  const existingColumns = new Set(Object.keys(data[0] || {}));
  const remainingVars = [...new Set(spec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v));
  remainingVars.forEach((varName) => {
    data.forEach((row) => {
      row[varName] = "DUMMY";
    });
  });
  return data;
}

// meddra型の項目にLLT名を格納する(Rのpopulate_meddra_fields()に対応。alias_nameによる絞り込みは
// 行わない点はAE/DM/DSと同じ)。populateGenericDummyFields()が先に走り、meddra型の列にも
// 一旦"DUMMY"を入れてしまうため、既存列かどうかで絞り込まず無条件に上書きする
// (R版のpopulate_meddra_fields()も列の存在有無を見ずに常に上書きしている)
function populateGenericMeddraFields(data, spec, meddraData, meddraSample) {
  const meddraVars = [...new Set(spec.filter((r) => r.field_type === "meddra").map((r) => r.cdisc_variable))];
  meddraVars.forEach((varName) => {
    const fixedCodes = [
      ...new Set(
        spec
          .filter((r) => r.field_type === "meddra" && r.cdisc_variable === varName && /^[0-9]{8}$/.test(r.default_value || ""))
          .map((r) => r.default_value)
      ),
    ];
    if (fixedCodes.length === 1) {
      const fixedRow = meddraData.find((r) => r.llt_code === fixedCodes[0]);
      const lltName = fixedRow ? fixedRow.llt_name : undefined;
      data.forEach((row) => {
        row[varName] = lltName;
      });
    } else {
      data.forEach((row, i) => {
        row[varName] = meddraSample[i].llt_name;
      });
    }
  });
  return data;
}

// field_type=="drug"に該当する変数名を抽出(Rのcompute_drug_vars()に対応)
function computeDrugVars(spec) {
  return [...new Set(spec.filter((r) => r.field_type === "drug").map((r) => r.cdisc_variable))];
}

// drugVars(field_type=="drug"な変数)のspec行が、1つでもdefault_value(固定コード)無し
// (=ランダムサンプリングされ、実際にwhoDrugIdfとの一致を確認する意味がある)場合はtrue。
// 全て固定コードで値が確定している場合はfalse(この場合、prefixDECOD列は生成しない)
// (Rのdrug_vars_need_decod()に対応)
function drugVarsNeedDecod(spec, drugVars) {
  const drugVarSet = new Set(drugVars);
  return spec.some((r) => r.field_type === "drug" && drugVarSet.has(r.cdisc_variable) && (r.default_value == null || r.default_value === ""));
}

// drug型の項目に、whoDrugIdf(drug_code/full_name_en/generic_name_enの配列)から薬剤名(full_name_en)を
// 格納する。default_valueが数値の場合はwhoDrugIdf$drug_codeとみなし対応するfull_name_enを固定値として
// 使う。それ以外はwhoDrugIdf$full_name_enからランダムにサンプリングする。alias_nameでスコープを絞る
// (同じcdisc_variableが別alias_nameで固定値/別のfield_typeとして定義されている場合、その行は変更しない)
// (Rのpopulate_drug_fields()に対応)
function populateDrugFields(data, spec, drugVars, whoDrugIdf) {
  const drugNames = [...new Set(whoDrugIdf.map((r) => r.full_name_en).filter((v) => v != null))];
  if (drugNames.length === 0) return data;
  drugVars.forEach((varName) => {
    const drugSpecRows = spec.filter((r) => r.field_type === "drug" && r.cdisc_variable === varName);
    const aliasNames = [...new Set(drugSpecRows.map((r) => r.alias_name))];
    aliasNames.forEach((an) => {
      const targetRows = data.filter((row) => row.alias_name === an);
      if (targetRows.length === 0) return;
      const defaultValues = [
        ...new Set(drugSpecRows.filter((r) => r.alias_name === an).map((r) => r.default_value).filter((v) => v != null && v !== "")),
      ];
      let fixedName = null;
      if (defaultValues.length === 1 && /^[0-9]+$/.test(defaultValues[0])) {
        const hit = whoDrugIdf.find((r) => r.drug_code === defaultValues[0] && r.full_name_en != null);
        if (hit) fixedName = hit.full_name_en;
      }
      targetRows.forEach((row) => {
        row[varName] = fixedName != null ? fixedName : sampleOne(drugNames);
      });
    });
  });
  return data;
}

// drug変数の値がwhoDrugIdfの薬剤名(full_name_en)に完全一致する場合、prefixDECOD(例: CMDECOD)に
// generic_name_enを格納する(一致しない場合はnull)。field_type=="drug"と定義されているalias_nameの
// 行だけを対象にする。drugVarsが複数ある場合は、最初に一致した変数の値を採用する
// (Rのadd_drug_decod()に対応)
function addDrugDecod(data, spec, drugVars, whoDrugIdf, prefix) {
  if (drugVars.length === 0 || !data[0]) return data;
  // 同じfull_name_enが複数行あり、一部だけgeneric_name_enが空のことがあるため、
  // generic_name_enが空でない行を優先して残してからルックアップを作る
  const lookup = {};
  [...whoDrugIdf]
    .filter((r) => r.full_name_en != null)
    .sort((a, b) => (a.generic_name_en == null ? 1 : 0) - (b.generic_name_en == null ? 1 : 0))
    .forEach((r) => {
      if (!(r.full_name_en in lookup)) lookup[r.full_name_en] = r.generic_name_en;
    });

  const decodVar = `${prefix}DECOD`;
  data.forEach((row) => {
    row[decodVar] = null;
  });
  drugVars.forEach((varName) => {
    const drugAliasNames = new Set(spec.filter((r) => r.field_type === "drug" && r.cdisc_variable === varName).map((r) => r.alias_name));
    data.forEach((row) => {
      if (row[decodVar] != null || !drugAliasNames.has(row.alias_name)) return;
      const matched = lookup[row[varName]];
      if (matched != null) row[decodVar] = matched;
    });
  });
  return data;
}

// データセット全体の通番を付与する(Rのadd_seq()に対応)
function addSeq(data, seqVar) {
  data.forEach((row, i) => {
    row[seqVar] = i + 1;
  });
  return data;
}

// other_domainsのdate型項目が、被験者の中止日(DISCONDTC)より後の値にならないようにする。
// 中止日情報がある被験者については、registrationStartDate〜中止日の範囲に収まるよう日付を
// 再生成する。中止日情報が無い(nullまたはdiscontinuationDateに無い)被験者は対象外
// (今まで通りregistrationStartDate〜今日の範囲のまま)。registrationStartDate > 中止日の場合
// (通常は起こらないはずだが念のため)は中止日そのものにする(Rのclamp_dates_to_discontinuation()に対応)。
// dateRefBoundsが渡された場合、他フィールド参照(同じ行)の下限/上限も尊重する。中止日超過に加えて、
// 同じ行の他日付フィールド(先に処理済み)との参照関係(ECSTDTC<=ECENDTC等)が崩れている行も再サンプル対象にする
// (参照先の値が先の反復で更新されている可能性があるため)。呼び出し側でlabelを跨ぐ行(label!=ref_label)を
// あらかじめ除いたdateRefBoundsを渡すこと(同じ行内の参照である前提のため)
function clampDatesToDiscontinuation(data, dateVars, registrationStartDate, discontinuationDate, dateRefBounds, existingData, presenceConditions) {
  if (!discontinuationDate || discontinuationDate.length === 0 || !data[0] || !("USUBJID" in data[0])) {
    return data;
  }
  const targetVars = sortDateVarsByDependency(dateVars.filter((v) => v in data[0]), dateRefBounds);
  if (targetVars.length === 0) return data;

  const today = new Date().toISOString().slice(0, 10);
  const disconByUsubjid = {};
  discontinuationDate.forEach((r) => {
    if (r.DISCONDTC != null && !(r.USUBJID in disconByUsubjid)) {
      disconByUsubjid[r.USUBJID] = r.DISCONDTC;
    }
  });

  const hasAliasName = "alias_name" in data[0];
  const hasLabel = hasAliasName && "label" in data[0];
  // labelがある場合(buildRepeatedDomain由来)は(alias_name, label)単位、無い場合(buildGenericDomain由来)は
  // alias_name単位をノードとして依存順序を組む。labelがある場合にalias_name単位のままだと、
  // 同一alias内で別labelを参照するケース(例: BLASTLE(005)がWBC(006)の値を参照)を
  // 見分けられず、両方が同じclampRows()呼び出しの中で独立に再サンプルされて値が食い違ってしまう
  const nodeKeyFor = (row) => (hasLabel ? `${row.alias_name}${row.label}` : row.alias_name);

  targetVars.forEach((varName) => {
    // targetRowsについて、discon超過・参照関係違反(min/max_ref。参照先の値はdataの"現在の"状態から
    // 都度計算するため、先に確定した値を反映できる)を判定し、該当行だけ新しい日付を再生成する
    const clampRows = (targetRows) => {
      if (!targetRows.some((v) => v)) return;
      const minRefVals = resolveDateRefBoundVals(data, dateRefBounds, varName, "min_date", existingData);
      const maxRefVals = resolveDateRefBoundVals(data, dateRefBounds, varName, "max_date", existingData);
      data.forEach((row, i) => {
        if (!targetRows[i]) return;
        const discon = disconByUsubjid[row.USUBJID];
        const current = row[varName];
        if (current == null) return;
        const disconOver = discon != null && current > discon;
        const minVal = minRefVals != null ? minRefVals[i] : null;
        const maxVal = maxRefVals != null ? maxRefVals[i] : null;
        const refViolation = (minVal != null && current < minVal) || (maxVal != null && current > maxVal);
        if (!disconOver && !refViolation) return;

        let lower = registrationStartDate;
        // RFSTDTC(症例登録日)は、この変数に明示的なmin_date参照(minVal)が無い場合のデフォルト下限としてのみ
        // 使う。明示的な参照(例: MHSTDTCのBRTHDTC基準)がある変数にまで一律にRFSTDTCを下限に加えると、
        // その変数本来の(RFSTDTCより前を許容する)意味を壊してしまうため
        if (minVal == null && row.RFSTDTC != null && row.RFSTDTC > lower) lower = row.RFSTDTC;
        if (minVal != null && minVal > lower) lower = minVal;
        // BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
        // RFSTDTCと異なり全行に適用する(buildRepeatedDomain内の日付生成ループと同じ理由)
        if (row.BRTHDTC != null && row.BRTHDTC > lower) lower = row.BRTHDTC;
        // hardUpper: 実際に許容できる上限(中止日、無ければ今日)。maxValがあればさらに絞る。
        // 通常の再クランプでは(discon未指定時)upperの初期値をcurrent(置き換え前の既存値)にしており、
        // ref_violationがmin側で発生した行はcurrent<lower(=minVal)が前提のため、upper<lowerは
        // 「今日/中止日を超えて本当に有効な範囲が無い」ことを意味しない(単に既存値を置き換えようと
        // しているだけ)。真に有効な範囲が無いかどうかはhardUpperとlowerで判定する
        let hardUpper = discon != null ? (discon > registrationStartDate ? discon : registrationStartDate) : today;
        if (maxVal != null && maxVal < hardUpper) hardUpper = maxVal;
        // minVal(date_ref_boundsの明示的なmin_date参照)が実際に効いてlowerを押し上げているときだけ
        // 「有効な範囲が無い」と判定する。RFSTDTC/BRTHDTCのデフォルト下限だけでhardUpperを超える場合
        // (例: 登録日が中止日より後という別の既存の実データ上の事情)は、この日付項目固有の問題では
        // ないため対象にせず、従来通りhardUpperまで切り詰める(下のelse節に委ねる)
        if (hardUpper < lower && minVal != null) {
          // 有効な日付範囲が存在しない場合(オフセット付き参照等により、参照先の値(+オフセット)が
          // 今日/中止日を超えてしまう。例: 移植150日後が評価日の下限だが、移植からまだ150日経っていない)、
          // 無理に未来日等で上書きせず未入力(null)に戻す。presenceの起点となっている同じ行の他フィールド
          // (例: FAORRES)があれば、そちらもnullにして連鎖的に後段のapply_presence_conditions()で
          // 下位項目も未入力扱いになるようにする
          row[varName] = null;
          (presenceConditions || []).forEach((pc) => {
            if (pc.cdisc_variable !== varName || pc.condition_type !== "not_blank" || pc.ref_cdisc_variable == null) return;
            if (pc.alias_name != null && pc.alias_name !== row.alias_name) return;
            if (pc.label != null && pc.label !== row.label) return;
            row[pc.ref_cdisc_variable] = null;
          });
          return;
        }
        // 通常のケース(有効な範囲は存在する): 上限はhardUpperを超えない範囲で、可能な限り既存値
        // (current)を尊重する(discon超過のみが理由の場合、既存値に近い日付に再サンプルするため)
        let upper = discon != null ? hardUpper : current;
        if (upper > hardUpper) upper = hardUpper;
        if (upper < lower) upper = lower;
        row[varName] = randomDateBetween(lower, upper);
      });
    };

    // 同じcdisc_variable名を、別のノード(alias_name、labelがあれば(alias_name, label))が参照している
    // 場合(例: inductionのSVSTDTCがprephaseのSVSTDTCを参照する、あるいは同一alias内でBLASTLE(005)が
    // WBC(006)の値を参照する)、参照先を先に確定させてから参照元を判定しないと、同じ呼び出しの中で
    // 参照先だけが後から再生成されて関係が崩れてしまう。そのため、そのようなノード間の依存がある場合だけ、
    // 依存関係順(参照されている側が先)にノードごとに処理する。依存が無ければ従来通り全行まとめて処理する。
    // labelがある場合は(alias_name, label)単位、無い場合はalias_name単位をノードとする(nodeKeyFor)。
    // labelがある場合にalias_name単位のままだと、同一alias内の別labelを参照するケースを見分けられず、
    // 参照元・参照先が同じclampRows()呼び出しに混ざって独立に再サンプルされ、値が食い違ってしまう
    const selfRefEdges = hasAliasName
      ? (dateRefBounds || [])
          .filter(
            (r) =>
              r.cdisc_variable === varName &&
              r.ref_cdisc_variable === varName &&
              r.alias_name != null &&
              r.ref_alias_name != null &&
              (hasLabel
                ? !(r.alias_name === r.ref_alias_name && r.label === r.ref_label)
                : r.alias_name !== r.ref_alias_name)
          )
          .reduce((acc, r) => {
            const fromKey = hasLabel ? r.alias_name + "" + r.label : r.alias_name;
            const toKey = hasLabel ? r.ref_alias_name + "" + r.ref_label : r.ref_alias_name;
            if (fromKey === toKey) return acc;
            if (!acc.some((e) => e.aliasName === fromKey && e.refAliasName === toKey)) {
              acc.push({ aliasName: fromKey, refAliasName: toKey });
            }
            return acc;
          }, [])
      : [];

    if (selfRefEdges.length === 0) {
      clampRows(data.map(() => true));
    } else {
      const allAliases = [...new Set(data.map((row) => nodeKeyFor(row)))];
      const orderedAliases = [];
      let remaining = allAliases;
      while (remaining.length > 0) {
        const remainingSet = new Set(remaining);
        const unresolved = new Set(
          selfRefEdges.filter((e) => remainingSet.has(e.refAliasName)).map((e) => e.aliasName)
        );
        const ready = remaining.filter((a) => !unresolved.has(a));
        if (ready.length === 0) {
          orderedAliases.push(...remaining);
          break;
        }
        orderedAliases.push(...ready);
        remaining = remaining.filter((a) => unresolved.has(a));
      }
      orderedAliases.forEach((key) => {
        clampRows(data.map((row) => nodeKeyFor(row) === key));
      });
    }
  });
  return data;
}

// 1つのalias_name内で、同じcdisc_variable名(例: ECSTDTC/ECENDTC)がlabel(繰り返しの1回分、例: 投与1回目・
// 2回目...)ごとに複数回登場し、「同じlabel内での開始日<=終了日」と「次のlabelの開始日>=前のlabelの終了日」
// のようなlabel内参照とlabelを跨ぐ参照が交互に連なるケース(例: EC複数回投与)向けの日付生成。
// dateRefBoundsのうちこのalias_nameかつchainVarsに関する行(label内・label跨ぎの両方)から
// (label, cdisc_variable)をノードとする依存グラフを作り、トポロジカル順に1ノードずつ値を確定させていく。
// buildRepeatedDomain()の通常の変数単位生成(同じ行=同じlabelの参照しか扱えない)を、
// このalias_nameのchainVarsに関してだけ上書きする形で使う(Rのregenerate_date_chain()に対応)
function regenerateDateChain(data, aliasNameVal, dateRefBounds, chainVars, registrationStartDate, disconByUsubjid) {
  const chainVarSet = new Set(chainVars);
  const bounds = dateRefBounds.filter(
    (r) => r.alias_name === aliasNameVal && chainVarSet.has(r.cdisc_variable) && r.label != null && r.ref_label != null
  );
  if (bounds.length === 0) return data;

  const nodeKey = (label, varName) => `${label}::${varName}`;
  const nodeMap = new Map();
  bounds.forEach((b) => {
    nodeMap.set(nodeKey(b.label, b.cdisc_variable), { label: b.label, varName: b.cdisc_variable });
    nodeMap.set(nodeKey(b.ref_label, b.ref_cdisc_variable), { label: b.ref_label, varName: b.ref_cdisc_variable });
  });

  const edgeFrom = bounds.map((b) => nodeKey(b.label, b.cdisc_variable));
  const edgeTo = bounds.map((b) => nodeKey(b.ref_label, b.ref_cdisc_variable));
  let remaining = [...nodeMap.keys()];
  const orderedKeys = [];
  while (remaining.length > 0) {
    const remainingSet = new Set(remaining);
    const unresolved = new Set(edgeFrom.filter((_, i) => remainingSet.has(edgeTo[i])));
    const ready = remaining.filter((k) => !unresolved.has(k));
    if (ready.length === 0) {
      orderedKeys.push(...remaining);
      break;
    }
    orderedKeys.push(...ready);
    remaining = remaining.filter((k) => !ready.includes(k));
  }

  const rowsByAliasLabel = new Map();
  data.forEach((row) => {
    if (row.alias_name !== aliasNameVal) return;
    const key = row.label;
    if (!rowsByAliasLabel.has(key)) rowsByAliasLabel.set(key, []);
    rowsByAliasLabel.get(key).push(row);
  });

  const today = new Date().toISOString().slice(0, 10);
  orderedKeys.forEach((key) => {
    const node = nodeMap.get(key);
    const varName = node.varName;
    if (!data[0] || !(varName in data[0])) return;
    const rows = rowsByAliasLabel.get(node.label);
    if (!rows || rows.length === 0) return;

    const minRow = bounds.find((b) => b.label === node.label && b.cdisc_variable === varName && b.bound_type === "min_date");
    const maxRow = bounds.find((b) => b.label === node.label && b.cdisc_variable === varName && b.bound_type === "max_date");
    const refValueFor = (row, refLabel, refVar) => {
      if (refLabel == null || refVar == null) return null;
      const refRows = rowsByAliasLabel.get(refLabel);
      if (!refRows) return null;
      const refRow = refRows.find((r) => r.USUBJID === row.USUBJID);
      return refRow ? refRow[refVar] : null;
    };

    rows.forEach((row) => {
      let lower = registrationStartDate;
      // RFSTDTC(症例登録日)は、この変数にchain内の明示的なmin_date参照(minRow)が無い場合の
      // デフォルト下限としてのみ使う(他の変数に明示的な参照がある場合にまで一律に加えると、
      // その変数本来の意味を壊してしまうため)
      if (minRow == null && row.RFSTDTC != null && row.RFSTDTC > lower) lower = row.RFSTDTC;
      if (minRow != null) {
        let refVal = refValueFor(row, minRow.ref_label, minRow.ref_cdisc_variable);
        if (refVal != null && minRow.offset_days) refVal = addDaysToDateString(refVal, minRow.offset_days);
        if (refVal != null && refVal > lower) lower = refVal;
      }
      // BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
      // RFSTDTCと異なり全行に適用する(buildRepeatedDomain内の日付生成ループと同じ理由)。
      // このchainに含まれるノード(例: WBCのように自分自身は他alias参照でchain対象外だが、
      // 同一alias内の他labelから参照されているためregenerateDateChain側でも再生成される変数)も、
      // ここで再生成される際にBRTHDTCより前にならないようにする
      if (row.BRTHDTC != null && row.BRTHDTC > lower) lower = row.BRTHDTC;
      let upper = today;
      const discon = disconByUsubjid ? disconByUsubjid[row.USUBJID] : null;
      if (discon != null && discon < upper) upper = discon;
      if (maxRow != null) {
        let refVal = refValueFor(row, maxRow.ref_label, maxRow.ref_cdisc_variable);
        if (refVal != null && maxRow.offset_days) refVal = addDaysToDateString(refVal, maxRow.offset_days);
        if (refVal != null && refVal < upper) upper = refVal;
      }
      if (upper < lower) upper = lower;
      row[varName] = randomDateBetween(lower, upper);
    });
  });
  return data;
}

// DM/AE/DSのような個別ロジックを持たないドメイン向けの汎用生成。
// alias_nameがmultiRecordAliasNamesに該当しない場合はUSUBJIDごとに1レコード、該当する場合
// (AE報告のように被験者ごとに複数件記録されうるシート)はAEドメインと同様、被験者に対して
// ランダムな件数(0件を含む)のレコードを作る(Rのbuild_generic_domain()に対応)。
// options: { addCodingBlock, builtDomains, cdiscVariableToPrefix, ageBounds, multiRecordAliasNames, activeSheetTable, whoDrugIdf, visitLookup, discontinuationDate }
function buildGenericDomain(dm, spec, prefix, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, numericBounds, fieldRefBounds, options) {
  const opts = options || {};
  const addCodingBlock = !!opts.addCodingBlock;
  const builtDomains = opts.builtDomains || {};
  const cdiscVariableToPrefix = opts.cdiscVariableToPrefix || {};
  const ageBounds = opts.ageBounds || [];
  const multiRecordAliasNames = opts.multiRecordAliasNames || [];
  const activeSheetTable = opts.activeSheetTable || null;
  const whoDrugIdf = opts.whoDrugIdf || null;
  const visitLookup = opts.visitLookup || null;
  const discontinuationDate = opts.discontinuationDate || null;
  const dateRefBoundsAll = opts.dateRefBounds || [];
  const isExclusive = !!opts.isExclusive;
  // existingData/finalizeは、依存関係の循環(prefix単位では循環に見えるがシート単位では循環でない
  // 参照チェーン)のためにこのprefixを複数wave(aliasNameの集合)に分けてビルドする場合に使う
  // (buildOtherDomains()のwave分割ロジックから渡される)。existingDataは前waveまでに確定済みの、
  // このprefixの行(alias_name列を保持したまま)。finalize=falseの場合はSEQ相当の連番付与や
  // 列順整理などprefix全体に対して1回だけ行うべき処理をスキップする。通常(wave分割しない場合)は
  // どちらも既定値のままでよく、挙動は従来と完全に同じになる
  const existingData = opts.existingData || null;
  const finalize = opts.finalize !== false;

  // presence_conditions/field_ref_bounds/age_bounds/date_ref_boundsは全ドメイン分を含む共通テーブルのため、
  // このドメイン自身のcdisc_variableに関する行だけに絞ってから使う
  const ownVars = new Set(spec.map((r) => r.cdisc_variable));
  const scopedPresenceConditions = (presenceConditions || []).filter((pc) => ownVars.has(pc.cdisc_variable));
  const scopedFieldRefBounds = (fieldRefBounds || []).filter((fb) => ownVars.has(fb.cdisc_variable));
  const scopedAgeBounds = ageBounds.filter((ab) => ownVars.has(ab.cdisc_variable));
  const scopedDateRefBounds = dateRefBoundsAll.filter((db) => ownVars.has(db.cdisc_variable));

  const aliasNames = [...new Set(spec.map((r) => r.alias_name))];
  const multiSet = new Set(multiRecordAliasNames);
  const singleAliasNames = aliasNames.filter((a) => !multiSet.has(a));
  const multiAliasNames = aliasNames.filter((a) => multiSet.has(a));

  // 同じcdisc_variableを複数のalias_nameが定義している場合、isExclusive===trueなら
  // resolvePreferredAliasName()で(その被験者について)候補から1つだけ選ぶ(例: DD。discon/withdrawalの
  // どちらか一方にしか本当の死因が記録されないような、真に排他的な事象を表すドメイン向け)。
  // isExclusive===false(既定)の場合は、有効な(USUBJID, alias_name)の組み合わせごとに1行を作る
  // (例: SV/PR/PC/CE/RS。被験者が実際に複数のalias_name(治療フェーズ・評価時点等)を経過することがあり、
  // そのどれもが独立して正しいレコードであるドメイン向け。互いに競合させず全て残す)
  let singleRows = [];
  if (singleAliasNames.length > 0) {
    let candidates;
    if (activeSheetTable) {
      const singleSet = new Set(singleAliasNames);
      candidates = activeSheetTable.filter((r) => singleSet.has(r.alias_name));
    } else {
      candidates = [];
      dm.forEach((dmRow) => {
        singleAliasNames.forEach((a) => candidates.push({ USUBJID: dmRow.USUBJID, alias_name: a }));
      });
    }
    singleRows = isExclusive
      ? resolvePreferredAliasName(candidates, scopedPresenceConditions, builtDomains, cdiscVariableToPrefix)
      : candidates;
  }

  const multiRows = [];
  multiAliasNames.forEach((an) => {
    const eligibleUsubjids = activeSheetTable
      ? [...new Set(activeSheetTable.filter((r) => r.alias_name === an).map((r) => r.USUBJID))]
      : dm.map((r) => r.USUBJID);
    if (eligibleUsubjids.length === 0) return;
    for (let i = 0; i < eligibleUsubjids.length; i += 1) {
      multiRows.push({ USUBJID: sampleOne(eligibleUsubjids), alias_name: an });
    }
  });

  const dmByUsubjid = {};
  dm.forEach((r) => {
    dmByUsubjid[r.USUBJID] = r;
  });

  let data = [...singleRows, ...multiRows].map((row) => ({
    STUDYID: dmByUsubjid[row.USUBJID] ? dmByUsubjid[row.USUBJID].STUDYID : null,
    DOMAIN: prefix,
    USUBJID: row.USUBJID,
    alias_name: row.alias_name,
  }));

  const spidVar = `${prefix}SPID`;
  data.forEach((row) => {
    row[spidVar] = row.alias_name;
  });
  data = applyMultiRecordSpid(data, spidVar, multiRecordAliasNames);

  // dateRefBoundsが他ドメインの日付列を参照する場合、populateGenericDateFields()より前にbuiltDomains
  // から該当列を結合しておく(そうしないと生成時点でref_cdisc_variableがdataの列に無く、下限/上限制約が
  // 適用されないまま日付が生成されてしまう)(Rのbuild_generic_domain()と同じ理由)
  const dateInjected = injectCrossDomainRefs(data, null, null, builtDomains, cdiscVariableToPrefix, null, scopedDateRefBounds, prefix);
  data = dateInjected.data;
  let dateInjectedCols = dateInjected.injectedCols;

  // 明示的なref()参照を持たない日付項目は、下のpopulateGenericDateFields()内でregistrationStartDate
  // (試験共通の定数)を下限にしてしまう。RFSTDTCを結合しておくことで、被験者本人の登録日を
  // デフォルトの下限にできるようにする
  const rfstdtcInjected = injectDmRfstdtc(data, builtDomains);
  data = rfstdtcInjected.data;
  if (rfstdtcInjected.injected) dateInjectedCols = [...dateInjectedCols, "RFSTDTC"];

  data = populateGenericChoiceFields(data, spec, numericBounds);
  data = populateGenericDateFields(data, spec, registrationStartDate, scopedDateRefBounds, existingData);
  const dateVars = [...new Set(spec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))];
  data = clampDatesToDiscontinuation(data, dateVars, registrationStartDate, discontinuationDate, scopedDateRefBounds, existingData, presenceConditions);
  // 同じcdisc_variableが複数alias(シート)にまたがる場合、シートの本来の並び順(sheet_seq)に沿うよう
  // alias単位でまとめて日付をシフトする。clampより後に行うことで、シフト結果を最終的な値として保つ
  // (この関数自体が被験者の中止日を上限にするため、clampが先に行った中止日調整と矛盾しない)
  data = reorderDatesBySheetSeq(data, dateVars, spec, registrationStartDate, discontinuationDate, scopedDateRefBounds);
  // reorderDatesBySheetSeqは同一alias内の複数labelをまとめて一律にシフトするため、他ドメイン参照
  // (scopedDateRefBounds)の下限/上限が再び崩れる場合がある。ここでもう一度clampして修復する
  // (discon超過判定は既に満たされているはずなので実質ref違反判定のみ効く)
  data = clampDatesToDiscontinuation(data, dateVars, registrationStartDate, discontinuationDate, scopedDateRefBounds, existingData, presenceConditions);
  data = populateDoseFields(data, spec);
  data = populateGenericDummyFields(data, spec);
  const seqVar = `${prefix}SEQ`;
  // wave分割していない(existingData無し)通常時は、従来通りここでSEQ相当の連番を振る。
  // wave分割時は、後段でexistingData(前wave分)と結合してから、finalize=trueのタイミングで
  // まとめて振る(通しの連番にするため)
  if (!existingData) {
    addSeq(data, seqVar);
  }

  const meddraVars = [...new Set(spec.filter((r) => r.field_type === "meddra").map((r) => r.cdisc_variable))];
  let codingCols = [];
  if (meddraVars.length > 0 && meddraData && data.length > 0) {
    const meddraSample = sampleMeddraRows(meddraData, data.length);
    data = populateGenericMeddraFields(data, spec, meddraData, meddraSample);
    if (addCodingBlock) {
      data = addAeMeddraCodingBlock(data, meddraSample, prefix);
      codingCols = MEDDRA_CODING_COLS.map((c) => prefix + c);
    }
  }

  // drug変数(field_type=="drug")には、whoDrugIdfから薬剤名をサンプリングして格納する。
  // alias_nameでスコープを絞る(同じcdisc_variableが別alias_nameで固定値等の場合はそちらを変更しない)
  const drugVars = computeDrugVars(spec);
  if (drugVars.length > 0 && whoDrugIdf) {
    data = populateDrugFields(data, spec, drugVars, whoDrugIdf);
  }

  // presence_conditions/field_ref_bounds/age_boundsが他ドメインの変数を参照している場合、
  // builtDomains(既に生成済みのドメイン)から値を結合してから条件を適用し、結合用に追加した列は最後に外す
  const injected = injectCrossDomainRefs(data, scopedPresenceConditions, scopedFieldRefBounds, builtDomains, cdiscVariableToPrefix, scopedAgeBounds, scopedDateRefBounds, prefix);
  data = injected.data;
  data = applyPresenceConditions(data, scopedPresenceConditions, cdiscVariableToPrefix);
  data = dropAllBlankRequiredRecords(data, [...ownVars], requiredVarInstances, prefix);
  data = applyFieldRefBounds(data, spec, scopedFieldRefBounds);
  data = applyAgeDateBounds(data, scopedAgeBounds, registrationStartDate);
  data.forEach((row) => {
    injected.injectedCols.forEach((c) => delete row[c]);
    dateInjectedCols.forEach((c) => delete row[c]);
  });

  // drug変数の値がwhoDrugIdfの薬剤名(full_name_en)に完全一致する場合、prefixDECODに
  // generic_name_enを格納する(presence_conditions等で値が変わった後の最終状態を見る)。
  // 全て固定コード(default_value)で値が確定している場合は、一致確認する意味が無いのでDECOD列自体を作らない
  if (drugVars.length > 0 && whoDrugIdf && drugVarsNeedDecod(spec, drugVars)) {
    data = addDrugDecod(data, spec, drugVars, whoDrugIdf, prefix);
  }

  data = addVisitColumns(data, visitLookup);

  // wave分割時(existingData指定あり)は、ここでこのwaveの新規行を前waveまでの確定済み行と結合する
  // (existingDataが無ければ何もしない=従来通り)。まだfinalizeでなければ、次waveのexistingDataとして
  // 使えるようalias_name列を保持したまま返す
  if (existingData) {
    data = [...existingData, ...data];
  }
  if (existingData && finalize) {
    addSeq(data, seqVar);
  }
  if (!finalize) {
    return data;
  }

  // alias_nameはここでは落とさない。finalize=trueはこのprefix自身の最後のwaveというだけで、
  // 他のleftover prefix(例: SV)がこのprefix(例: RS)をより後のwaveでalias単位に参照する場合が
  // あるため、buildOtherDomains側の最終ステップ(builtDomainsを返り値に変換する直前)で全prefix
  // まとめて落とすまで保持しておく必要がある(実際に発生したバグ: RSが一度きりのwaveでfinalize=true
  // になりここでalias_nameを落としてしまい、後からRSをalias単位で参照するSV(hdm等)の
  // injectCrossDomainRefsが参照先aliasを絞り込めず、別aliasの値を誤って拾ってしまっていた)

  // 列順を STUDYID/DOMAIN/USUBJID/prefixSEQ/prefixSPID -> meddra項目 -> コーディングブロック -> その他 に整理する
  const frontCols = ["STUDYID", "DOMAIN", "USUBJID", seqVar, spidVar, ...meddraVars, ...codingCols];
  const allColsSet = new Set();
  data.forEach((row) => Object.keys(row).forEach((c) => allColsSet.add(c)));
  const middleCols = [...allColsSet].filter((c) => !frontCols.includes(c));
  const orderedCols = [...frontCols.filter((c) => allColsSet.has(c)), ...middleCols];

  return data.map((row) => {
    const newRow = {};
    orderedCols.forEach((c) => {
      newRow[c] = c in row ? row[c] : null;
    });
    return newRow;
  });
}

// specの中で、同じ(alias_name, cdisc_variable)の組が複数の異なるlabelを持つか(=繰り返しフィールドか)を判定する
// (Rのhas_repeated_labels()に対応)
function hasRepeatedLabels(spec) {
  const counts = new Map();
  spec.forEach((r) => {
    if (r.label == null) return;
    const key = `${r.alias_name}|${r.cdisc_variable}`;
    if (!counts.has(key)) counts.set(key, new Set());
    counts.get(key).add(r.label);
  });
  return [...counts.values()].some((labels) => labels.size > 1);
}

// TR/LBのように、同じcdisc_variableが同じalias_name内で複数のlabel(繰り返しフィールド)に対応するドメイン向け。
// USUBJID×(alias_name, label)の組み合わせごとに1レコード作り、各変数は自分のlabelに対応するspec行だけを見て
// 値を生成する(対応するlabelが無ければnullのまま)。radio_button/check_box/date/meddra/drug/dose/dummyに
// 対応する(Rのbuild_repeated_domain()に対応)。
// options: { addCodingBlock, builtDomains, cdiscVariableToPrefix, ageBounds, multiRecordAliasNames, activeSheetTable, whoDrugIdf, visitLookup, discontinuationDate }
function buildRepeatedDomain(dm, spec, prefix, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, options) {
  const opts = options || {};
  const addCodingBlock = !!opts.addCodingBlock;
  const builtDomains = opts.builtDomains || {};
  const cdiscVariableToPrefix = opts.cdiscVariableToPrefix || {};
  const ageBounds = opts.ageBounds || [];
  const multiRecordAliasNames = opts.multiRecordAliasNames || [];
  const activeSheetTable = opts.activeSheetTable || null;
  const whoDrugIdf = opts.whoDrugIdf || null;
  const visitLookup = opts.visitLookup || null;
  const discontinuationDate = opts.discontinuationDate || null;
  const dateRefBoundsAll = opts.dateRefBounds || [];
  // existingData/finalizeの意味はbuildGenericDomain()と同じ(prefix単位では循環に見える依存関係を
  // 複数waveに分けて解決するため。通常は既定値のままでよく、挙動は従来と完全に同じになる)
  const existingData = opts.existingData || null;
  const finalize = opts.finalize !== false;
  const drugNames = whoDrugIdf ? [...new Set(whoDrugIdf.map((r) => r.full_name_en).filter((v) => v != null))] : [];

  const ownVars = new Set(spec.map((r) => r.cdisc_variable));
  const scopedPresenceConditions = (presenceConditions || []).filter((pc) => ownVars.has(pc.cdisc_variable));
  const scopedAgeBounds = ageBounds.filter((ab) => ownVars.has(ab.cdisc_variable));
  const scopedDateRefBounds = dateRefBoundsAll.filter((db) => ownVars.has(db.cdisc_variable));

  const repeatUnitsMap = new Map();
  spec.forEach((r) => {
    if (r.label == null) return;
    const key = `${r.alias_name}|${r.label}`;
    if (!repeatUnitsMap.has(key)) repeatUnitsMap.set(key, { alias_name: r.alias_name, label: r.label });
  });
  const repeatUnits = [...repeatUnitsMap.values()];

  const dmByUsubjid = {};
  dm.forEach((r) => {
    dmByUsubjid[r.USUBJID] = r;
  });

  let data = [];
  if (activeSheetTable) {
    activeSheetTable.forEach((row) => {
      repeatUnits.forEach((ru) => {
        if (ru.alias_name === row.alias_name) {
          data.push({ USUBJID: row.USUBJID, alias_name: ru.alias_name, label: ru.label });
        }
      });
    });
  } else {
    dm.forEach((dmRow) => {
      repeatUnits.forEach((ru) => {
        data.push({ USUBJID: dmRow.USUBJID, alias_name: ru.alias_name, label: ru.label });
      });
    });
  }
  data.forEach((row) => {
    row.STUDYID = dmByUsubjid[row.USUBJID] ? dmByUsubjid[row.USUBJID].STUDYID : null;
    row.DOMAIN = prefix;
  });

  const spidVar = `${prefix}SPID`;
  data.forEach((row) => {
    row[spidVar] = row.alias_name;
  });
  data = applyMultiRecordSpid(data, spidVar, multiRecordAliasNames);

  // dateRefBoundsが他ドメインの日付列を参照する場合、このあとの日付生成より前にbuiltDomainsから
  // 該当列を結合しておく(そうしないと生成時点でref_cdisc_variableがdataの列に無く、下限/上限制約が
  // 適用されないまま日付が生成されてしまう)(Rのbuild_repeated_domain()と同じ理由)
  const dateInjected = injectCrossDomainRefs(data, null, null, builtDomains, cdiscVariableToPrefix, null, scopedDateRefBounds, prefix);
  data = dateInjected.data;
  let dateInjectedCols = dateInjected.injectedCols;

  // 明示的なref()参照を持たない日付項目は、下の日付生成でregistrationStartDate(試験共通の定数)を
  // 下限にしてしまう。RFSTDTCを結合しておくことで、被験者本人の登録日をデフォルトの下限にできるようにする
  const rfstdtcInjected = injectDmRfstdtc(data, builtDomains);
  data = rfstdtcInjected.data;
  if (rfstdtcInjected.injected) dateInjectedCols = [...dateInjectedCols, "RFSTDTC"];

  // BRTHDTC(生年月日)より前の日付が生成されないよう、dmから直接結合しておく。乳児コホート等では
  // BRTHDTCがregistrationStartDate/RFSTDTCより後になり得るため、明示的なref()参照の有無によらず
  // 常に適用すべき下限(生物学的制約)として扱う。追加した列は他のinjectedColsと同様、最後に取り除く。
  // BRTHDTCが既に列として存在する場合(直前のinjectCrossDomainRefs()が、dateRefBoundsで特定の
  // alias/labelだけを対象にBRTHDTCを部分的に結合済みのケース。例: FAのbaselineアリアスのFADTCが
  // BRTHDTCを下限参照している場合、そのaliasの行だけ埋まる)は、そのまま素通りすると他のalias
  // (例: osteonecrosis1)の行がBRTHDTC未設定のまま残ってしまう。列自体は残しつつ、
  // 未充填(null/undefined)の行だけUSUBJID単位で埋める
  if (dm[0] && "BRTHDTC" in dm[0]) {
    const hasBrthdtcCol = data[0] && "BRTHDTC" in data[0];
    if (!hasBrthdtcCol || data.some((row) => row.BRTHDTC == null)) {
      const brthdtcByUsubjid = {};
      dm.forEach((row) => {
        if (!(row.USUBJID in brthdtcByUsubjid)) brthdtcByUsubjid[row.USUBJID] = row.BRTHDTC;
      });
      data.forEach((row) => {
        if (row.BRTHDTC == null) {
          row.BRTHDTC = row.USUBJID in brthdtcByUsubjid ? brthdtcByUsubjid[row.USUBJID] : null;
        }
      });
      if (!hasBrthdtcCol) dateInjectedCols = [...dateInjectedCols, "BRTHDTC"];
    }
  }

  const existingColumns = new Set(Object.keys(data[0] || {}));
  let targetVars = [...new Set(spec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v));

  // date型の変数同士が、同じ行(同一alias_name×label)の中でvalidate_date_after_or_equal_to/
  // validate_date_before_or_equal_to(他フィールド参照)によって数珠つなぎに依存し合う場合、参照先が
  // 先に生成されていないと値を引けない。scopedDateRefBounds(このドメインのdate_vars同士の依存だけ)を
  // 使って依存が無いものから順に並べ替える(Rのbuild_repeated_domain()内のソート処理に対応)
  {
    const dateVarsForSort = [...new Set(spec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))].filter((v) =>
      targetVars.includes(v)
    );
    const sortedDateVars = sortDateVarsByDependency(dateVarsForSort, scopedDateRefBounds);
    const sortedDateVarSet = new Set(sortedDateVars);
    targetVars = [...targetVars.filter((v) => !sortedDateVarSet.has(v)), ...sortedDateVars];
  }

  // (alias_name, label)ごとにグループ化しておく(組み合わせ数×行数のスキャンを避けるため)
  const groups = new Map();
  data.forEach((row) => {
    const key = `${row.alias_name}|${row.label}`;
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key).push(row);
  });

  const doseChoices = ["50", "100", "150", "200", "250", "300", "400", "500"];
  const today = new Date().toISOString().slice(0, 10);

  targetVars.forEach((varName) => {
    // var_nameにvalidate_date_after_or_equal_to/validate_date_before_or_equal_to(他フィールド参照)が
    // あり、かつ参照先が既に生成済み(このforEachの前の反復で追加された変数)なら、そのフィールド名(列名)を
    // 使う。無ければ一律の範囲で生成する(Rのbuild_repeated_domain()と同じ理由)
    const dateMinRow = scopedDateRefBounds.find((r) => r.cdisc_variable === varName && r.bound_type === "min_date");
    const dateMaxRow = scopedDateRefBounds.find((r) => r.cdisc_variable === varName && r.bound_type === "max_date");
    const varSpec = spec.filter((r) => r.cdisc_variable === varName);
    const specByGroup = new Map();
    varSpec.forEach((r) => {
      const key = `${r.alias_name}|${r.label}`;
      if (!specByGroup.has(key)) {
        specByGroup.set(key, { fieldType: r.field_type, defaultValue: r.default_value, codes: new Set(), isInvisibleAny: false, isRequiredAny: false });
      }
      const g = specByGroup.get(key);
      g.codes.add(r.code != null ? r.code : r.default_value);
      if (r.is_invisible) g.isInvisibleAny = true;
      if (r.is_required) g.isRequiredAny = true;
    });

    groups.forEach((rows, key) => {
      const g = specByGroup.get(key);
      if (!g) {
        rows.forEach((row) => {
          row[varName] = null;
        });
        return;
      }
      if (g.fieldType == null) {
        // このUSUBJID×(alias_name,label)の組み合わせでは、この変数自体が定義されていない
        // (同じalias_name内の他labelで定義された別変数がこの繰り返し単位を作っただけ)。
        // Rのbuild_repeated_domain()のis.na(ft)分岐と同じくnullのままにする("DUMMY"にはしない)
        rows.forEach((row) => {
          row[varName] = null;
        });
        return;
      }
      let codes = [...g.codes];
      if (!g.isRequiredAny && !g.isInvisibleAny) {
        codes = [...new Set([...codes, ""])];
      }
      if (g.fieldType === "radio_button" || g.fieldType === "check_box") {
        if (codes.length === 0) {
          rows.forEach((row) => {
            row[varName] = null;
          });
        } else if (g.fieldType === "check_box") {
          const values = sampleCheckBoxValuesWithCoverage(codes, rows.length);
          rows.forEach((row, i) => {
            row[varName] = values[i];
          });
        } else {
          const values = sampleValuesWithCoverage(codes, rows.length);
          rows.forEach((row, i) => {
            row[varName] = values[i];
          });
        }
      } else if (g.fieldType === "date") {
        rows.forEach((row) => {
          let lower = registrationStartDate;
          // 明示的なmin_date参照(dateMinRow)があっても、labelを跨ぐ連鎖等で行によっては参照先の値が
          // まだ無い(例: EC複数回投与の1回目=連鎖の先頭)ことがある。そのような行にだけRFSTDTCを
          // デフォルト下限として補う(参照値がある行では、その変数本来の意味を尊重してRFSTDTCは加えない)。
          // ref('sheet_alias', N)+N.days/-N.daysの符号付き日数オフセット(dateMinRow.offset_days。
          // 無指定ならnull=0として扱う)を参照先の値に加味する
          const rawMinRefVal = dateMinRow != null ? row[dateMinRow.ref_cdisc_variable] : null;
          const refVal = rawMinRefVal != null && dateMinRow.offset_days ? addDaysToDateString(rawMinRefVal, dateMinRow.offset_days) : rawMinRefVal;
          if (refVal == null && row.RFSTDTC != null && row.RFSTDTC > lower) lower = row.RFSTDTC;
          if (refVal != null && refVal > lower) {
            lower = refVal;
          }
          // BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
          // RFSTDTCと異なり全行に適用する(乳児コホート等ではBRTHDTCがregistrationStartDate/RFSTDTCより
          // 後になり得るため、それらのデフォルト下限だけでは生年月日より前の日付が生成されてしまう)
          if (row.BRTHDTC != null && row.BRTHDTC > lower) lower = row.BRTHDTC;
          let upper = today;
          const rawMaxRefVal = dateMaxRow != null ? row[dateMaxRow.ref_cdisc_variable] : null;
          const maxRefVal = rawMaxRefVal != null && dateMaxRow.offset_days ? addDaysToDateString(rawMaxRefVal, dateMaxRow.offset_days) : rawMaxRefVal;
          if (maxRefVal != null && maxRefVal < upper) {
            upper = maxRefVal;
          }
          // refVal(date_ref_boundsの明示的なmin_date参照)が実際に効いてlowerを押し上げているときだけ
          // 「有効な範囲が無い」と判定する。RFSTDTC/BRTHDTCのデフォルト下限だけで今日を超える場合
          // (この日付項目固有の参照とは無関係な実データ上の事情)は対象にせず、従来通り今日に切り詰める
          if (upper < lower && refVal != null) {
            // 有効な日付範囲が存在しない(オフセット付き参照等により下限が上限(今日/他の参照)を
            // 超えてしまう。例: 移植150日後が評価日の下限だが、移植からまだ150日経っていない)場合、
            // 無理に未来日等を生成せず、この項目を未入力(null)のままにする。あわせて、この変数の
            // presenceを「同じ行の他フィールドが値を持つこと」で条件づけているpresence_conditions
            // (not_blank条件。例: "ORRES.present?"→FADTCの下限参照)があれば、そのref_cdisc_variable
            // (presenceの起点となっている側、例: FAORRES)も同じ行でnullにする。これにより、後段の
            // apply_presence_conditions()で連鎖的に下位の項目(GRADE等)も正しく未入力扱いになる
            row[varName] = null;
            (presenceConditions || []).forEach((pc) => {
              if (pc.cdisc_variable !== varName || pc.condition_type !== "not_blank" || pc.ref_cdisc_variable == null) return;
              if (pc.alias_name != null && pc.alias_name !== row.alias_name) return;
              if (pc.label != null && pc.label !== row.label) return;
              row[pc.ref_cdisc_variable] = null;
            });
          } else {
            if (upper < lower) upper = lower;
            row[varName] = randomDateBetween(lower, upper);
          }
        });
      } else if (g.fieldType === "meddra") {
        const dv = g.defaultValue;
        if (dv != null && /^[0-9]{8}$/.test(dv)) {
          const hit = meddraData.find((r) => r.llt_code === dv);
          const lltName = hit ? hit.llt_name : null;
          rows.forEach((row) => {
            row[varName] = lltName;
          });
        } else {
          const sample = sampleMeddraRows(meddraData, rows.length);
          rows.forEach((row, i) => {
            row[varName] = sample[i].llt_name;
          });
        }
      } else if (g.fieldType === "drug") {
        const dv = g.defaultValue;
        let fixedName = null;
        if (dv != null && /^[0-9]+$/.test(dv) && whoDrugIdf) {
          const hit = whoDrugIdf.find((r) => r.drug_code === dv && r.full_name_en != null);
          if (hit) fixedName = hit.full_name_en;
        }
        if (fixedName != null) {
          rows.forEach((row) => {
            row[varName] = fixedName;
          });
        } else if (drugNames.length > 0) {
          rows.forEach((row) => {
            row[varName] = sampleOne(drugNames);
          });
        } else {
          rows.forEach((row) => {
            row[varName] = null;
          });
        }
      } else if (/DOSE$/.test(varName)) {
        rows.forEach((row) => {
          row[varName] = sampleOne(doseChoices);
        });
      } else {
        rows.forEach((row) => {
          row[varName] = "DUMMY";
        });
      }
    });
  });

  const dateVars = [...new Set(spec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))];

  // 1つのalias内でcdisc_variable名がlabel(繰り返しの1回分)を跨いで連鎖する行(例: 次回投与の開始日が
  // 前回投与の終了日を参照する)がある場合、通常の変数単位生成ではlabelを跨いだ参照を扱えないため、
  // regenerateDateChain()でそのalias・その変数だけ生成し直す(Rのbuild_repeated_domain()と同じ理由)。
  // refCdiscVariableも自ドメインのdateVars内にある場合のみ対象にする(regenerateDateChain()は同じ
  // dataフレーム内にrefLabelの行がある前提のため、refCdiscVariableが他ドメインの変数(例: RSDTCが
  // CMSTDTCを参照)の場合はrefLabelの行が存在せず機能しない。他ドメイン参照はinjectCrossDomainRefs()で
  // 既にrefCdiscVariable列自体がdataに結合済みなので、通常の変数単位生成・下のclampDatesToDiscontinuation
  // のref違反判定に任せればよい)。aliasName === refAliasNameも必須にする(同一ドメイン内で別alias(シート)を
  // 参照するケース、例: evaluationtp1のLBDTCがinductionlabのLBDTCを参照、はlabelが一致しないだけでこの
  // フィルタに誤って引っかかっていた。regenerateDateChain()は同じaliasのlabel行しか見ないため参照先が
  // 見つからずref無視のまま生成され、下限が緩すぎる日付が生成されてしまうバグがあった)
  const chainBounds = scopedDateRefBounds.filter(
    (r) =>
      dateVars.includes(r.cdisc_variable) &&
      dateVars.includes(r.ref_cdisc_variable) &&
      r.label != null &&
      r.ref_label != null &&
      r.label !== r.ref_label &&
      r.alias_name === r.ref_alias_name
  );
  if (chainBounds.length > 0) {
    const chainVars = [...new Set([...chainBounds.map((r) => r.cdisc_variable), ...chainBounds.map((r) => r.ref_cdisc_variable)])].filter(
      (v) => dateVars.includes(v)
    );
    let disconByUsubjid = null;
    if (discontinuationDate && discontinuationDate.length > 0 && data[0] && "USUBJID" in data[0]) {
      disconByUsubjid = {};
      discontinuationDate.forEach((r) => {
        if (r.DISCONDTC != null && !(r.USUBJID in disconByUsubjid)) {
          disconByUsubjid[r.USUBJID] = r.DISCONDTC;
        }
      });
    }
    const chainAliasNames = [...new Set(chainBounds.map((r) => r.alias_name))];
    chainAliasNames.forEach((aliasVal) => {
      // scopedDateRefBoundsではなくchainBoundsを渡す。regenerateDateChain内でcdisc_variable単独でしか
      // 絞り込んでいないと、alias_name===aliasValかつcdisc_variableがchainVarsに含まれるが実際は
      // 他ドメイン参照(ref_cdisc_variableがchainVars外、例: WBCのLBDTCがSVSTDTCを参照)の行まで
      // 拾ってしまい、そのref先がこのalias内に存在しないため値を解決できないままminRow相当が非nullに
      // なり、本来効くはずのRFSTDTC等のデフォルト下限が適用されなくなる(実際に発生したバグ:
      // WBC/BLASTLEの等号制約が崩れた)
      data = regenerateDateChain(data, aliasVal, chainBounds, chainVars, registrationStartDate, disconByUsubjid);
    });
  }
  // clampDatesToDiscontinuation()はselfRefEdges(labelがあれば(alias_name, label)単位)で依存順に処理するため、
  // labelを跨ぐ連鎖(regenerateDateChain()で既に処理済みの同一alias内のものも含む)をそのまま渡してよい。
  // 中止日超過などでchain regen後に値が再サンプルされる場合でも、参照元・参照先の順序が正しく守られる
  data = clampDatesToDiscontinuation(data, dateVars, registrationStartDate, discontinuationDate, scopedDateRefBounds, existingData, presenceConditions);
  // 同じcdisc_variableが複数alias(シート)にまたがる場合(例: 来院ごとに繰り返すEC/LB/VS)、
  // シートの本来の並び順(sheet_seq)に沿うようalias単位でまとめて日付をシフトする。alias内の関係
  // (同じ行の開始日<=終了日、labelを跨ぐ連鎖)は保ったまま動くため、上のregenerateDateChain()・
  // clampより後に行う(この関数自体が中止日を上限にするため矛盾しない)
  data = reorderDatesBySheetSeq(data, dateVars, spec, registrationStartDate, discontinuationDate, scopedDateRefBounds);
  // reorderDatesBySheetSeqは同一alias内の複数labelをまとめて一律にシフトするため、参照関係
  // (scopedDateRefBounds)の下限/上限が再び崩れる場合がある。ここでもう一度clampして修復する
  // (discon超過判定は既に満たされているはずなので実質ref違反判定のみ効く)
  data = clampDatesToDiscontinuation(data, dateVars, registrationStartDate, discontinuationDate, scopedDateRefBounds, existingData, presenceConditions);

  let codingCols = [];
  if (addCodingBlock) {
    const meddraTypeVars = [...new Set(spec.filter((r) => r.field_type === "meddra").map((r) => r.cdisc_variable))].filter((v) =>
      Object.prototype.hasOwnProperty.call(data[0] || {}, v)
    );
    if (meddraTypeVars.length > 0) {
      const lltLookup = {};
      meddraData.forEach((r) => {
        if (!(r.llt_name in lltLookup)) lltLookup[r.llt_name] = r;
      });
      const meddraSample = data.map((row) => {
        let lltName = null;
        for (let i = 0; i < meddraTypeVars.length; i += 1) {
          if (row[meddraTypeVars[i]] != null) {
            lltName = row[meddraTypeVars[i]];
            break;
          }
        }
        // lltNameがmeddraデータ上のllt_nameと完全一致しない場合(例: radio_button型のMHTERMのように
        // 選択肢の文言そのものを使っている場合)、LLTCD等のコード系列はnullのままにしつつ、
        // LLT列自体は元のlltNameを保持する(Rのadd_meddra_coding_block()がleft_joinでllt_name列を
        // 保持したまま他列だけNAにするのと同じ挙動)
        return lltLookup[lltName] || { llt_name: lltName };
      });
      data = addAeMeddraCodingBlock(data, meddraSample, prefix);
      codingCols = MEDDRA_CODING_COLS.map((c) => prefix + c);
    }
  }

  // presence_conditions/age_boundsが他ドメインの変数を参照している場合、builtDomainsから値を結合してから
  // 条件を適用し、結合用に追加した列は最後に外す(field_ref_boundsはRのbuild_repeated_domain()と同様に対象外)
  const injected = injectCrossDomainRefs(data, scopedPresenceConditions, null, builtDomains, cdiscVariableToPrefix, scopedAgeBounds, scopedDateRefBounds, prefix);
  data = injected.data;
  data = applyPresenceConditions(data, scopedPresenceConditions, cdiscVariableToPrefix);
  data = dropAllBlankRequiredRecords(data, targetVars, requiredVarInstances, prefix);
  data = applyAgeDateBounds(data, scopedAgeBounds, registrationStartDate);
  data.forEach((row) => {
    injected.injectedCols.forEach((c) => delete row[c]);
    dateInjectedCols.forEach((c) => delete row[c]);
  });

  // drug変数の値がwhoDrugIdfの薬剤名(full_name_en)に完全一致する場合、prefixDECODに
  // generic_name_enを格納する。全て固定コード(default_value)で値が確定している場合は
  // DECOD列自体を作らない
  const drugVarsInData = computeDrugVars(spec).filter((v) => data[0] && v in data[0]);
  if (drugVarsInData.length > 0 && whoDrugIdf && drugVarsNeedDecod(spec, drugVarsInData)) {
    data = addDrugDecod(data, spec, drugVarsInData, whoDrugIdf, prefix);
  }

  data = addVisitColumns(data, visitLookup);

  const seqVar = `${prefix}SEQ`;

  // wave分割時(existingData指定あり)は、ここでこのwaveの新規行を前waveまでの確定済み行と結合する
  // (existingDataが無ければ何もしない=従来通り)。まだfinalizeでなければ、次waveのexistingDataとして
  // 使えるようそのまま返す(SEQ相当の連番はfinalizeのタイミングでまとめて振る)
  if (existingData) {
    data = [...existingData, ...data];
  }
  if (!finalize) {
    return data;
  }
  addSeq(data, seqVar);

  // alias_name/labelはここでは残す(オーケストレーター(buildOtherDomains)が他ドメイン参照の突き合わせキーとして使い、
  // 最終出力を作る段階で取り除く。Rのbuild_repeated_domain()と同じ)
  const frontCols = ["STUDYID", "DOMAIN", "USUBJID", seqVar, spidVar, ...codingCols];
  const allColsSet = new Set();
  data.forEach((row) => Object.keys(row).forEach((c) => allColsSet.add(c)));
  const middleCols = [...allColsSet].filter((c) => !frontCols.includes(c));
  const orderedCols = [...frontCols.filter((c) => allColsSet.has(c)), ...middleCols];

  return data.map((row) => {
    const newRow = {};
    orderedCols.forEach((c) => {
      newRow[c] = c in row ? row[c] : null;
    });
    return newRow;
  });
}

// cdiscVariableValuesに含まれるprefixのうち、excludePrefixes(既定でDM/AE/DS)を除いた全てについて、
// ドメイン間の依存関係(presence_conditions/field_ref_bounds/age_boundsが他ドメインを参照する箇所)を
// トポロジカルソートで解決した順に、has_repeated_labels(またはrepeatedPrefixesで明示指定)に応じて
// buildGenericDomain/buildRepeatedDomainを呼び分けて生成する。prefixをキーにしたオブジェクトで返す
// (Rのbuild_other_domains()に対応するが、AEリンクブロック・apply_orres_populatorsはまだ未対応)。
// options: { excludePrefixes, codingBlockPrefixes, repeatedPrefixes, builtDomains, ageBounds,
//            multiRecordAliasNames, activeSheetTable, whoDrugIdf, visitLookup, discontinuationDate }
function buildOtherDomains(dm, cdiscVariableValues, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, numericBounds, fieldRefBounds, options) {
  const opts = options || {};
  const excludePrefixes = new Set(opts.excludePrefixes || ["DM", "AE", "DS"]);
  const codingBlockPrefixes = new Set(opts.codingBlockPrefixes || ["MH"]);
  const forceRepeatedPrefixes = new Set(opts.repeatedPrefixes || []);
  const exclusivePrefixes = new Set(opts.exclusivePrefixes || ["DD"]);
  const builtDomains = Object.assign({}, opts.builtDomains || {});
  const ageBounds = opts.ageBounds || [];
  const multiRecordAliasNames = opts.multiRecordAliasNames || [];
  const activeSheetTable = opts.activeSheetTable || null;
  const whoDrugIdf = opts.whoDrugIdf || null;
  const visitLookup = opts.visitLookup || null;
  const discontinuationDate = opts.discontinuationDate || null;
  const dateRefBounds = opts.dateRefBounds || [];
  const preBuiltDomains = opts.preBuiltDomains || {};
  const preBuiltAliasNames = opts.preBuiltAliasNames || {};

  const prefixes = [...new Set(cdiscVariableValues.map((r) => r.prefix))].filter((p) => !excludePrefixes.has(p));
  const cdiscVariableToPrefix = buildCdiscVariableToPrefix(cdiscVariableValues);
  const edges = buildCrossPrefixEdges(presenceConditions, fieldRefBounds, cdiscVariableToPrefix, ageBounds, dateRefBounds);
  const { ordered: orderedPrefixes, remaining: leftoverPrefixes } = topoSortPrefixesWithLeftover(prefixes, edges);

  const buildOnePrefix = (prefix, spec, existingData, finalize) => {
    const addCodingBlock = codingBlockPrefixes.has(prefix);
    const buildOptions = {
      addCodingBlock,
      builtDomains,
      cdiscVariableToPrefix,
      ageBounds,
      multiRecordAliasNames,
      activeSheetTable,
      whoDrugIdf,
      visitLookup,
      discontinuationDate,
      dateRefBounds,
      isExclusive: exclusivePrefixes.has(prefix),
      existingData: existingData || null,
      finalize: finalize !== false,
    };
    const fullSpec = cdiscVariableValues.filter((r) => r.prefix === prefix);
    return forceRepeatedPrefixes.has(prefix) || hasRepeatedLabels(fullSpec)
      ? buildRepeatedDomain(dm, spec, prefix, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, buildOptions)
      : buildGenericDomain(dm, spec, prefix, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, numericBounds, fieldRefBounds, buildOptions);
  };

  // 循環に関与しないprefix(大多数)は、従来通り1回の呼び出しでビルドする(コードパス・挙動とも変更なし)。
  // preBuiltDomains(呼び出し元がこの関数より前に一部のaliasNameだけ先行生成済みのprefix。例: AEの
  // AESTDTCが参照するMH(registration)ブロック。MHSTDTCが他ドメインに依存せず、AEより前に単独で
  // 生成できるため)が指定されている場合、そのaliasNameをspecから除いた上でexistingData(=
  // preBuiltDomains[prefix])に続けて残りのaliasNameを生成する(先行生成済みの値をそのまま使い、
  // 二重生成による値の食い違いを避ける)
  orderedPrefixes.forEach((prefix) => {
    let spec = cdiscVariableValues.filter((r) => r.prefix === prefix);
    if (preBuiltDomains[prefix]) {
      const excludeAliases = new Set(preBuiltAliasNames[prefix] || []);
      spec = spec.filter((r) => !excludeAliases.has(r.alias_name));
      builtDomains[prefix] = buildOnePrefix(prefix, spec, preBuiltDomains[prefix], true);
    } else {
      builtDomains[prefix] = buildOnePrefix(prefix, spec);
    }
  });

  // 循環に関与するprefix(leftoverPrefixes)は、prefix単位ではなくシート(aliasName)単位で依存関係を
  // 解決し、複数wave(aliasNameのまとまり)に分けてビルドする。実際には循環ではなく、単に同じprefix内の
  // 別シートを介した参照チェーンが、prefix単位の粗い依存判定では循環に見えていただけ、というケースが
  // ほとんど(例: SV(hr3fisrt)→RS→LB→FA→SV(prephase))(Rのbuild_other_domains()に対応)
  if (leftoverPrefixes.length > 0) {
    const leftoverPrefixSet = new Set(leftoverPrefixes);
    const leftoverValues = cdiscVariableValues.filter((r) => leftoverPrefixSet.has(r.prefix));
    const aliasNodesMap = new Map();
    leftoverValues.forEach((r) => {
      const key = `${r.prefix}|${r.alias_name}`;
      if (!aliasNodesMap.has(key)) aliasNodesMap.set(key, { prefix: r.prefix, alias_name: r.alias_name });
    });
    const aliasNodes = [...aliasNodesMap.values()];
    const aliasEdges = buildAliasLevelEdges(presenceConditions, fieldRefBounds, cdiscVariableToPrefix, ageBounds, dateRefBounds).filter(
      (e) => leftoverPrefixSet.has(e.fromPrefix) && leftoverPrefixSet.has(e.toPrefix)
    );
    const orderedPairs = topoSortPrefixAliases(aliasNodes, aliasEdges);

    if (orderedPairs.length > 0) {
      const runs = [];
      orderedPairs.forEach((pair, i) => {
        if (i === 0 || pair.prefix !== orderedPairs[i - 1].prefix) {
          runs.push({ prefix: pair.prefix, aliasNames: new Set() });
        }
        runs[runs.length - 1].aliasNames.add(pair.alias_name);
      });
      const lastRunIndexByPrefix = {};
      runs.forEach((run, i) => {
        lastRunIndexByPrefix[run.prefix] = i;
      });

      runs.forEach((run, i) => {
        const spec = leftoverValues.filter((r) => r.prefix === run.prefix && run.aliasNames.has(r.alias_name));
        const isLast = i === lastRunIndexByPrefix[run.prefix];
        builtDomains[run.prefix] = buildOnePrefix(run.prefix, spec, builtDomains[run.prefix], isLast);
      });
    }
  }

  const result = {};
  prefixes.forEach((prefix) => {
    result[prefix] = (builtDomains[prefix] || []).map((row) => {
      const { alias_name, label, ...rest } = row;
      return rest;
    });
  });
  return result;
}

// gatedVars(presence_conditionsで条件付けされている変数)が全てnullの行を除外する。
// DD(死因)のように、DDTEST/DDTESTCDのような固定値の列は常に埋まっているため、
// 「ドメインの全列がnull」ではなく「条件付きの列(例: DDORRES)が全てnull」で判定する必要がある。
// gatedVarsが空、またはdomainに1つも存在しない場合は何もしない(Rのdrop_empty_domain_rows()に対応)
function dropEmptyDomainRows(domain, gatedVars) {
  const relevantVars = (gatedVars || []).filter((v) => domain[0] && v in domain[0]);
  if (relevantVars.length === 0) return domain;
  return domain.filter((row) => relevantVars.some((v) => row[v] != null && row[v] !== ""));
}

// ae/sae_reportのように、AE報告と同じフォーム上の他prefixブロック(例: FA)は、既にpopulateLinkedBlocks側で
// (AE報告と同じ行として)生成済みのため、buildOtherDomains側では二重生成しないよう該当のprefix/alias_name
// をcdiscVariableValuesから除外する(Rのexclude_ae_linked_prefixes()に対応)
function excludeAeLinkedPrefixes(cdiscVariableValues, aeLinkedDomains) {
  const excludedPairs = new Set();
  Object.keys(aeLinkedDomains || {}).forEach((prefix) => {
    const aliasNames = new Set((aeLinkedDomains[prefix] || []).map((r) => r.alias_name));
    aliasNames.forEach((a) => excludedPairs.add(`${prefix}|${a}`));
  });
  return cdiscVariableValues.filter((r) => !excludedPairs.has(`${r.prefix}|${r.alias_name}`));
}

// AE報告と同じ行として生成したリンク先ブロック(例: FA)を、対応するother_domainsにマージする
// (Rのmerge_linked_domains()に対応)
function mergeLinkedDomains(otherDomains, aeLinkedDomains) {
  Object.keys(aeLinkedDomains || {}).forEach((linkedPrefix) => {
    const fragment = aeLinkedDomains[linkedPrefix].map((row) => {
      const { alias_name, ...rest } = row;
      return rest;
    });
    const merged = linkedPrefix in otherDomains ? [...otherDomains[linkedPrefix], ...fragment] : fragment;

    const seqVar = `${linkedPrefix}SEQ`;
    addSeq(merged, seqVar);

    const frontCols = ["STUDYID", "DOMAIN", "USUBJID", seqVar, `${linkedPrefix}SPID`];
    const allColsSet = new Set();
    merged.forEach((row) => Object.keys(row).forEach((c) => allColsSet.add(c)));
    const middleCols = [...allColsSet].filter((c) => !frontCols.includes(c));
    const orderedCols = [...frontCols.filter((c) => allColsSet.has(c)), ...middleCols];

    otherDomains[linkedPrefix] = merged.map((row) => {
      const newRow = {};
      orderedCols.forEach((c) => {
        newRow[c] = c in row ? row[c] : null;
      });
      return newRow;
    });
  });
  return otherDomains;
}
